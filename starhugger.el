;;; starhugger.el --- LLM/AI-powered text & code completion client  -*- lexical-binding: t; -*-

;; Version: 0.7.0-git
;; Package-Requires: ((emacs "28.2") (compat "29.1.4.0") (dash "2.18.0") (s "1.13.1") (spinner "1.7.4") (request "0.3.2"))
;; Keywords: completion, convenience, languages
;; Homepage: https://gitlab.com/daanturo/starhugger.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LLM/AI-powered text & code completion client.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'project)

(require 'compat)
(require 'dash)
(require 's)
(require 'request)

(defgroup starhugger nil
  "LLM-powered code completion client."
  :group 'external)

;;;; Helpers

(cl-defmacro starhugger--with-buffer-scrolling (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
Like `with-current-buffer', but allow scrolling the visible
window of BUFFER-OR-NAME when at the buffer end, if any."
  (declare (debug t) (indent defun))
  `(-let* ((wd (get-buffer-window ,buffer-or-name t)))
     (cond
      (wd
       (with-selected-window wd
         ;; disallow scrolling when visibly not at end of buffer
         (-let* ((eob-flag (eobp))
                 (pt0 (point)))
           (unwind-protect
               (progn
                 ,@body)
             (unless eob-flag
               (goto-char pt0))))))
      (t
       (with-current-buffer ,buffer-or-name
         ,@body)))))

(cl-defun starthugger--join-strings-no-repeat-separator (components &optional (separator "/"))
  "Join COMPONENTS with SEPARATOR in between, without repeating it.
Joining [\"a\" \"b\"], [\"a\" \"/b\"], [\"a/\" \"b\"], [\"a/\" \"/b\"]
all gives \"a/b\"."
  (named-let
      recur
      ((comps (seq-rest components))
       (retval
        (ignore-error (args-out-of-range)
          (seq-first components))))
    (-let* ((new
             (ignore-error (args-out-of-range)
               (seq-first comps)))
            (suf-old (string-suffix-p separator retval))
            (pre-new (string-prefix-p separator new)))
      (cond
       ((seq-empty-p comps)
        retval)
       ((and suf-old pre-new)
        (recur
         (seq-rest comps) (concat retval (substring new (length separator)))))
       ((or suf-old pre-new)
        (recur (seq-rest comps) (concat retval new)))
       (:else
        (recur (seq-rest comps) (concat retval separator new)))))))

(defun starhugger--project-root (&optional dir)
  "Return the root directory of DIR (defaults to current directory)'s project.
Return nil if not found."
  (-some--> (project-current nil dir) (project-root it)))

(defvar starhugger--guess-language-id--cache
  (make-hash-table :test #'equal))
(defun starhugger--guess-language-id ()
  (with-memoization (gethash
                     major-mode
                     starhugger--guess-language-id--cache)
    (cond
     ((stringp mode-name)
      (replace-regexp-in-string "[ \t]" "-" (downcase mode-name)))
     (:else
      (replace-regexp-in-string
       "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))))

(defmacro my-with-live-buffer-or-current (buffer &rest body)
  "If BUFFER is live, execute BODY in it, else in current buffer."
  (declare (debug t) (indent defun))
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer
         ,@body)
     (progn
       ,@body)))

;;;; Making requests

(defcustom starhugger-model-id "qwen2.5-coder:7b-base"
  "The language model's name on selected platform."
  :type 'string)

(defcustom starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log parsed responses."
  :type 'string)

;; Admittedly a wrong name
(defcustom starhugger-max-prompt-length (* 1024 8)
  "Max length of the code in current buffer to send.
Doesn't count fills tokens and maybe the context."
  :type 'natnum)

;; TODO: logging is currently a mess, tidy and make them useful

(defvar starhugger--log-buffer (format " *%s-log*" 'starhugger)
  "Buffer name to log things, hidden by default.")

(defun starhugger--log (&rest args)
  (unless (get-buffer starhugger--log-buffer)
    (with-current-buffer (get-buffer-create starhugger--log-buffer)
      (dlet ((view-inhibit-help-message t))
        (read-only-mode))))
  (with-current-buffer starhugger--log-buffer
    (-let* ((pt0 (and (not (eobp)) (point))))
      (dlet ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "(%F %T) "))
        (dolist (obj (-interpose " " args))
          (insert (format "%s" obj)))
        (insert "\n")
        (when pt0
          (goto-char pt0))))))

(declare-function spinner-start "spinner")
(defvar spinner-types)
(defvar starhugger--spinner-added-type nil)
(defun starhugger--spinner-start ()
  (unless starhugger--spinner-added-type
    (require 'spinner)
    (push '(starhugger . ["⭐" "🌟"]) spinner-types)
    (setq starhugger--spinner-added-type t))
  (spinner-start 'starhugger 3))

(defcustom starhugger-enable-spinner t
  "Show spinner when fetching interactively."
  :type 'boolean)

;; WHY isn't this documented?!
(defvar url-http-end-of-headers)


(defcustom starhugger-max-new-tokens nil
  "When a number, set it to max_new_tokens.
It can be a list of two natural numbers: the number of tokens to fetch
when called automatically and the number of token to fetch when called
interactively."
  :type '(choice natnum (list natnum natnum)))

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (-->
   object (apply #'json-serialize it args) (encode-coding-string it 'utf-8)))

(defvar starhugger-debug nil)
(defvar starhugger--last-returned-request nil
  "Last returned request info, for debugging.")
(defvar starhugger--last-sent-request nil
  "Last sent request info, for debugging.")

(defvar starhugger-before-request-hook '()
  "Hook run before making an HTTP request.")

(defvar starhugger--running-request-table--table nil
  "Keys are buffers.")
(defun starhugger--running-request-table ()
  (unless starhugger--running-request-table--table
    (setq starhugger--running-request-table--table
          (make-hash-table :test #'equal)))
  starhugger--running-request-table--table)


(defun starhugger--record-propertize (str)
  (propertize str 'face '(:foreground "yellow" :weight bold)))

(defvar starhugger--record-heading-beg "#*> ")

(cl-defun starhugger--record-generated (prompt parsed-response-list &rest args &key display &allow-other-keys)
  (-let* ((buf
           (or (get-buffer starhugger-generated-buffer)
               (prog1 (get-buffer-create starhugger-generated-buffer)
                 (with-current-buffer starhugger-generated-buffer
                   (setq-local outline-regexp
                               (regexp-quote starhugger--record-heading-beg))
                   (setq-local window-point-insertion-type t)
                   (read-only-mode))))))
    (starhugger--with-buffer-scrolling
      buf
      (dlet ((inhibit-read-only t))

        (goto-char (point-max))
        (insert
         (starhugger--record-propertize
          (concat starhugger--record-heading-beg "API INPUT: ")))
        (insert (format "(info: %S)" args) "\n\n")
        (when (stringp prompt)
          (insert prompt))
        (insert "\n\n")

        (if (equal parsed-response-list '())
            (insert
             (starhugger--record-propertize
              (concat starhugger--record-heading-beg "OUTPUT from API: None!\n")))
          (-let* ((lst (ensure-list parsed-response-list)))
            (--each lst
              (insert
               (starhugger--record-propertize
                (format
                 "%sAPI OUTPUT #%d/%d:"
                 starhugger--record-heading-beg (+ 1 it-index) (length lst))))
              (insert "\n" it "\n\n"))))

        (insert "\n\n\n")))
    (when display
      (save-selected-window
        (pop-to-buffer buf)))
    buf))

(defcustom starhugger-strip-prompt-before-insert nil
  "Whether to remove the prompt in the parsed response before inserting.
Enable this when the return_full_text parameter isn't honored."
  :type 'boolean)

(defun starhugger-post-process-instruct-markdown-code-block (text &rest _)
  (-let* ((parts (split-string text "^```\\([^`]\\|$\\).*" nil "[ \t\n]?")))
    (cond
     ;; A single code block in this format, extract it:
     ;; {optional talk}\n```.*\n{code block}\n```\n{optional talk}
     ((= 3 (length parts))
      (--> (nth 1 parts)
           ;; If the model doesn't understand and generates MD code block
           ;; anyway, the preceding spaces are usually redundant
           (string-trim-left it "[ \t]+")))
     (:else
      text))))

(defcustom starhugger-post-process-chain
  `(
    ;; Remove reasoning models's thoughts
    ("\\`[ \t\n\r]*<think>[^z-a]*?</think>[ \t\n\r]*" "")
    starhugger-post-process-instruct-markdown-code-block)
  "List of post-processing steps for model responses.
Each element can be:
- A variadic function whose arguments are (generated-text &rest _) and returns
  the processed text.
- A list of 2 strings: `replace-regexp-in-string''s first 2 arguments."
  :type '(repeat (choice function (list regexp string))))

(defcustom starhugger-fill-in-the-middle t
  "Enable using code from both before and after point as prompt.

If set to the symbol '`instruct' and
`starhugger-completion-backend-function', is appropriate, `starhugger'
will try to construct a prompt that tells the instruction-tuned language
model to fill in the spot, enable making use of chat models that don't
support FIM.  Note that this prompt may not always success.  See also
the option `starhugger-instruct-make-messages-prompt-function' to customize
prompting."
  :type '(choice boolean (const instruct)))

(defcustom starhugger-prompt-after-point-fraction (/ 1.0 4)
  "The length fraction that code after point (suffix) should take."
  :type 'float)

(defcustom starhugger-retry-temperature-range '(0.0 1.0)
  "The lower and upper bound of random temperature when retrying.
A single number means just use it without generating.  nil means don't
set temperature at all."
  :type '(list float float))

(defun starhugger--retry-temperature ()
  (if (and (listp starhugger-retry-temperature-range)
           (not (seq-empty-p starhugger-retry-temperature-range)))
      (-let* (((lo hi) starhugger-retry-temperature-range))
        (--> (cl-random 1.0) (* it (- hi lo)) (+ lo it)))
    starhugger-retry-temperature-range))

(defun starhugger--log-before-request (url data &rest args)
  (setq starhugger--last-sent-request (list :url url :data data))
  (when starhugger-debug
    (dlet ((starhugger--log-buffer " *starhugger sent request data*"))
      (apply #'starhugger--log url data args))))

(defun starhugger--log-after-request
    (request-data &optional error-flag &rest args)
  (setq starhugger--last-returned-request request-data)
  (when (or starhugger-debug error-flag)
    (apply #'starhugger--log starhugger--last-returned-request args)))

(defcustom starhugger-notify-request-error t
  "Whether to notify if an error was thrown when the request is completed."
  :type 'boolean)

(defun starhugger--without-notify-request-error--a (func &rest args)
  (dlet ((starhugger-notify-request-error nil))
    (apply func args)))

(defun starhugger--request-el--silent-sentinel-a (fn proc event &rest args)
  (cond
   ((member event '("interrupt" "interrupt\n"))
    (dlet ((request-message-level -1))
      (apply fn proc event args)))
   (:else
    (apply fn proc event args))))

(cl-defun starhugger--request-el-request (url &rest settings &key type parser error &allow-other-keys)
  "Wrapper around (`request' URL @SETTINGS) that silence errors when aborting.
See https://github.com/tkf/emacs-request/issues/226."
  (declare (indent defun))
  (-let* ((req
           (apply #'request
                  url
                  :type (or type "POST")
                  :parser
                  (or parser (lambda () (json-parse-buffer :object-type 'alist)))
                  :error (or error #'ignore) settings)))
    (add-function
     :around
     (process-sentinel (get-buffer-process (request-response--buffer req)))
     #'starhugger--request-el--silent-sentinel-a)
    req))

;;;;; Backends

;;;;;; Ollama

(defcustom starhugger-ollama-generate-api-url
  "http://localhost:11434/api/generate"
  "Ollama API's generation endpoint."
  :type 'string)

(defcustom starhugger-ollama-additional-parameter-alist
  '((options) (stream . :false))
  "Ollama API's advanced parameters.
See https://github.com/ollama/ollama/blob/main/docs/api.md#parameters."
  :type 'alist)

(cl-defun starhugger-ollama-completion-api (prompt callback &rest args &key
                                                   model force-new max-new-tokens
                                                   prefix suffix &allow-other-keys)
  (-let* ((model (or model starhugger-model-id))
          (sending-data
           (starhugger--json-serialize
            `((prompt . ,(if suffix prefix prompt))
              (model . ,model)
              ,@(and suffix `((suffix . ,suffix)))
              (options
               ,@(and max-new-tokens `((num_predict . ,max-new-tokens)))
               ,@(and force-new
                      starhugger-retry-temperature-range
                      `((temperature . ,(starhugger--retry-temperature))))
               ,@(alist-get 'options starhugger-ollama-additional-parameter-alist))
              (stream . :false)
              ,@starhugger-ollama-additional-parameter-alist))))
    (starhugger--log-before-request
     starhugger-ollama-generate-api-url sending-data)
    (-let* ((request-obj
             (starhugger--request-el-request
               starhugger-ollama-generate-api-url
               :data sending-data
               :complete
               (cl-function
                (lambda (&rest
                         returned
                         &key
                         data
                         error-thrown
                         response
                         &allow-other-keys)
                  (-let* ((generated-lst
                           (if error-thrown
                               '()
                             (-some-->
                                 data
                               (list (alist-get 'response it))))))
                    (starhugger--log-after-request
                     (list
                      :response-content returned
                      :send-data sending-data
                      :response-status
                      (request-response-status-code response))
                     error-thrown)
                    (funcall callback
                             generated-lst
                             :model model
                             :error
                             (and error-thrown
                                  `((error-thrown ,error-thrown)
                                    (data ,data)))))))))
            (request-buf (request-response--buffer request-obj))
            (request-proc (get-buffer-process request-buf))
            (cancel-fn (lambda () (request-abort request-obj))))
      (list
       :cancel-fn cancel-fn
       :process request-proc
       :request-response request-obj))))

;;;;;; OpenAI-compatible API

;; Default value: Ollama's local server
(defcustom starhugger-openai-compat-url "http://localhost:11434/v1"
  "Base URL for OpenAI-compatible API.
See also `starhugger-openai-compat-base-completions-endpoint'"
  :type 'string)

(defcustom starhugger-openai-compat-api-key nil
  "API key for authentication at your provider.
This string must be unibyte, ensure that before dynamically setting
with (`encode-coding-string' ... \\='utf-8)."
  :type 'string)

(defvar starhugger-openai-compat-base-completions-endpoint "/completions"
  "Base models's completions endpoint of `starhugger-openai-compat-url'.")

(defcustom starhugger-openai-compat-base-completions-parameter-alist '((stream . :false))
  "Parameters for the /completions endpoint.
An alist (Info node `(elisp) Association Lists') to be converted to JSON
with `json-serialize'.  See
https://platform.openai.com/docs/api-reference/completions/create."
  :type 'alist)

(defvar starhugger-openai-compat-chat-completions-endpoint "/chat/completions"
  "Chat models's completions endpoint of `starhugger-openai-compat-url'.")

(defcustom starhugger-openai-compat-chat-completions-parameter-alist '((stream . :false))
  "Parameters for the /chat/completions endpoint.
See https://platform.openai.com/docs/api-reference/chat/create."
  :type 'alist)

(cl-defun starhugger-openai-compat-api-base-completions
    (prompt
     callback
     &rest
     args
     &key
     model
     force-new
     max-new-tokens
     prefix
     suffix
     &allow-other-keys)
  (-let* ((model (or model starhugger-model-id))
          (url
           (starthugger--join-strings-no-repeat-separator
            (list
             starhugger-openai-compat-url
             starhugger-openai-compat-base-completions-endpoint)))
          (sending-data
           (starhugger--json-serialize
            `((prompt .
                      ,(or prefix prompt))
              (model . ,model)
              ,@(and suffix `((suffix . ,suffix)))
              ,@(and max-new-tokens `((max_tokens . ,max-new-tokens)))
              ,@(and force-new
                     starhugger-retry-temperature-range
                     `((temperature . ,(starhugger--retry-temperature))))
              (echo . :false)
              (stream . :false)
              ,@starhugger-openai-compat-base-completions-parameter-alist))))
    (starhugger--log-before-request url sending-data)
    (-let* ((request-obj
             (starhugger--request-el-request url
               :header
               `(("Authorization" .
                  ,(format "Bearer %s" starhugger-openai-compat-api-key)))
               :data sending-data
               :complete
               (cl-function
                (lambda (&rest
                         returned
                         &key
                         data
                         error-thrown
                         response
                         &allow-other-keys)
                  (-let* ((generated-lst
                           (if error-thrown
                               '()
                             (-some-->
                                 data
                               (alist-get 'choices it)
                               (seq-map
                                (lambda (choice)
                                  (alist-get 'text choice))
                                it)))))
                    (starhugger--log-after-request
                     (list
                      :response-content returned
                      :send-data sending-data
                      :response-status
                      (request-response-status-code response))
                     error-thrown)
                    (funcall callback
                             generated-lst
                             :model model
                             :error
                             (and error-thrown
                                  `((error-thrown ,error-thrown)
                                    (data ,data)))))))))
            (request-buf (request-response--buffer request-obj))
            (request-proc (get-buffer-process request-buf))
            (cancel-fn (lambda () (request-abort request-obj))))
      (list
       :cancel-fn cancel-fn
       :process request-proc
       :request-response request-obj))))

(cl-defun starhugger-openai-compat-api-chat-completions
    (_prompt
     callback
     &rest
     args
     &key
     messages
     model
     force-new
     max-new-tokens
     &allow-other-keys)
  (-let* ((model (or model starhugger-model-id))
          (url
           (starthugger--join-strings-no-repeat-separator
            (list
             starhugger-openai-compat-url
             starhugger-openai-compat-chat-completions-endpoint)))
          (sending-data
           (starhugger--json-serialize
            `((messages . ,messages)
              (model . ,model)
              ,@(and max-new-tokens `((max_tokens . ,max-new-tokens)))
              ,@(and force-new
                     starhugger-retry-temperature-range
                     `((temperature . ,(starhugger--retry-temperature))))
              (stream . :false)
              ,@starhugger-openai-compat-chat-completions-parameter-alist))))
    (starhugger--log-before-request url sending-data)
    (-let* ((request-obj
             (starhugger--request-el-request url
               :header
               `(("Authorization" .
                  ,(format "Bearer %s" starhugger-openai-compat-api-key)))
               :data sending-data
               :complete
               (cl-function
                (lambda (&rest
                         returned
                         &key
                         data
                         error-thrown
                         response
                         &allow-other-keys)
                  (-let* ((generated-lst
                           (if error-thrown
                               '()
                             (-some-->
                                 data
                               (alist-get 'choices it)
                               (seq-map
                                (lambda (choice)
                                  (map-nested-elt choice '(message content)))
                                it)))))
                    (starhugger--log-after-request
                     (list
                      :response-content returned
                      :send-data sending-data
                      :response-status
                      (request-response-status-code response))
                     error-thrown)
                    (funcall callback
                             generated-lst
                             :model model
                             :error
                             (and error-thrown
                                  `((error-thrown ,error-thrown)
                                    (data ,data)))))))))
            (request-buf (request-response--buffer request-obj))
            (request-proc (get-buffer-process request-buf))
            (cancel-fn (lambda () (request-abort request-obj))))
      (list
       :cancel-fn cancel-fn
       :process request-proc
       :request-response request-obj))))

;;;;; Completion

(defcustom starhugger-completion-backend-function
  #'starhugger-openai-compat-api-base-completions
  "The backend for code suggestion.
The function accepts those arguments: prompt (string), callback
function (that accepts these arguments and should be supplied:
the model's list of generated strings, optional keywords such as
`:error'), and optional keyword(s): `:parameters', `:options':
alist of parameters and options, respectively to pass to the
backend's model; and other optional arguments: `:force-new',
`:max-new-tokens', `:num-return-sequences'.

It must return a plist that contains: `:process': the process to
be terminate to cancel the request, whose `process-sentinel' can
be decorated; optionally `:cancel-fn': a function that terminates
the running request when called, (calling `:cancel-fn' is
prioritized over stopping `:process')."
  :type 'function
  :options
  '(starhugger-openai-compat-api-chat-completions
    starhugger-openai-compat-api-base-completions
    starhugger-ollama-completion-api))

(defun starhugger--post-process-do (str &optional chain)
  (named-let
      recur ((retval str) (chain (or chain starhugger-post-process-chain)))
    (-let* ((op (car chain)))
      (cond
       ((seq-empty-p chain)
        retval)
       ((and (listp op) (= 2 (length op)) (-every #'stringp op))
        (recur
         (replace-regexp-in-string (nth 0 op) (nth 1 op) retval) (cdr chain)))
       ((functionp op)
        (recur (funcall op retval) (cdr chain)))))))

(cl-defun starhugger--query-internal (prompt callback &rest args &key display spin backend caller &allow-other-keys)
  "CALLBACK is called with the generated text list and a plist.
PROMPT is the prompt to use. DISPLAY is whether to display the
generated text in a buffer. SPIN is whether to show a spinner.
ARGS are the arguments to pass to the BACKEND (or
`starhugger-completion-backend-function')"
  (run-hooks 'starhugger-before-request-hook)
  (-let* ((orig-buf (current-buffer))
          (spin-obj
           (and spin starhugger-enable-spinner (starhugger--spinner-start)))
          (backend (or backend starhugger-completion-backend-function))
          (request-record nil))
    (letrec ((returned
              (apply backend
                     prompt
                     (cl-function
                      (lambda (content-choices
                               &rest cb-args &key error &allow-other-keys)
                        (my-with-live-buffer-or-current orig-buf
                          (cl-callf
                              (lambda (lst) (delete request-record lst))
                              (gethash orig-buf (starhugger--running-request-table)
                                       '()))
                          (when spin-obj
                            (funcall spin-obj))
                          (-let* ((err-str (format "%S" error))
                                  (processed-content-choices
                                   (-map
                                    #'starhugger--post-process-do
                                    content-choices)))
                            (starhugger--record-generated
                             prompt
                             content-choices
                             :parameters args
                             :other-info cb-args
                             :backend backend
                             :display display)
                            (when (and error starhugger-notify-request-error)
                              (message "`starhugger' response error: %s"
                                       err-str))
                            (apply callback
                                   processed-content-choices
                                   cb-args)))))
                     args)))
      (setq request-record (append returned `(:caller ,caller)))
      (push
       request-record (gethash orig-buf (starhugger--running-request-table) '()))
      request-record)))


;;;; Overlay inline suggestion

(defvar-local starhugger--overlay nil)

(defface starhugger-inline-suggestion-face
  '((t
     :foreground "gray"
     :italic t
     ;; override `:underline' so that the flymake (or any other on-the-fly
     ;; checker) errors at point won't make the suggestion unreadable because of
     ;; the intrusive underlines, `:extend' prevents drawn underlines after line
     ;; ends
     :underline nil
     :extend t))
  "Face for suggestion overlays.")

;; We may as well use a ring (`make-ring'), but it doesn't have a built-in way
;; to modify elements in-place
(defvar-local starhugger--suggestion-list '()
  "List of recently fetched suggestions along with internal state.
Recent suggestions are added to the beginning.")


(defcustom starhugger-suggestion-list-size 32
  "Maximum number of saved suggestions in current buffer.
Note that this includes all recently fetched suggestions so not
all of them are relevant all the time."
  :type 'natnum)


(defcustom starhugger-dismiss-suggestion-after-change t
  "Whether to clear the overlay when text changes and not partially accepted."
  :type 'boolean)

(defvar-local starhugger--inline-inhibit-changing-overlay nil)

(defun starhugger-inlining--after-change-h (&optional _beg _end _old-len)
  (when (and this-command
             (not starhugger--inline-inhibit-changing-overlay)
             starhugger-dismiss-suggestion-after-change)
    (starhugger-dismiss-suggestion)))

(define-minor-mode starhugger-inlining-mode
  "Not meant to be called normally.
When this minor mode is off, the overlay must not be shown."
  :global nil
  :lighter " 🌠"
  :keymap `( ;
            (,(kbd "<remap> <keyboard-quit>") . starhugger-dismiss-suggestion)
            ;;
            )
  (if starhugger-inlining-mode
      (progn
        (add-hook 'after-change-functions #'starhugger-inlining--after-change-h nil t))
    (progn
      (remove-hook 'after-change-functions #'starhugger-inlining--after-change-h t)
      (when (overlayp starhugger--overlay)
        (delete-overlay starhugger--overlay)))))

(defun starhugger--ensure-inlininng-mode (&optional off)
  (if off
      (when starhugger-inlining-mode
        (starhugger-inlining-mode 0))
    (unless starhugger-inlining-mode
      (starhugger-inlining-mode))))

(defcustom starhugger-numbers-of-suggestions-to-fetch '(2 3)
  "List of (natural) numbers of suggestions to fetch.

The first number is the number of suggestions to fetch when
`starhugger-trigger-suggestion' is called automatically.

The second number is the number of suggestions to fetch when
`starhugger-trigger-suggestion' is called interactively.

It can also be a single number, in which case the first number is
the same as the second number."
  :type '(choice natnum (list natnum natnum)))

(defun starhugger--current-overlay-suggestion ()
  (overlay-get starhugger--overlay 'starhugger-ovlp-current-suggestion))

(defvar starhugger-at-suggestion-map (make-sparse-keymap)
  "Use `starhugger-inline-menu-item' instead!
This doesn't work at the end of buffer.

Keymap used when at the beginning of suggestion overlay.")
(make-obsolete-variable 'starhugger-at-suggestion-map nil "0.1.18")

(defun starhugger--update-overlay (suggt &optional orig-pt)
  "Update overlay to display SUGGT after ORIG-PT.
ORIG-PT defaults to current point, when supplying it with a
non-nil (numeric) value, mark SUGGT and ORIG-PT as the original
ones."
  (-let* ((beg-pt (or orig-pt (point)))
          (suggt*
           (-->
            (propertize suggt
                        'face 'starhugger-inline-suggestion-face
                        ;; allow placing the cursor before the overlay when
                        ;; 'after-string
                        'cursor t)
            ;; WORKAROUND: when the suggestions begins with a newline, the point
            ;; will be placed at the start of the first non-newline character,
            ;; therefore won't stay at the current position visually, workaround
            ;; this by prefixing with a space
            (if (and (< 0 (length suggt)) (= ?\n (aref suggt 0)))
                (concat " " it)
              it))))
    (overlay-put starhugger--overlay 'starhugger-ovlp-current-suggestion suggt)
    (when orig-pt
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-suggestion suggt)
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-position orig-pt))
    ;; at end of buffer, 'display doesn't show anything because
    ;; `overlay-starr'=`overlay-end'
    (if (<= (point-max) beg-pt)
        (progn
          (when (overlay-get starhugger--overlay 'display)
            (overlay-put starhugger--overlay 'display nil))
          (overlay-put starhugger--overlay 'after-string suggt*))
      ;; currently I can't find a way to to achieve this:

      ;; 〈before〉|〈overlay〉〈after〉

      ;; so the workaround is too concatenate "overlay" and "after" and and put
      ;; the overlay on "after"
      (progn
        (when (overlay-get starhugger--overlay 'after-string)
          (overlay-put starhugger--overlay 'after-string nil))
        (overlay-put
         starhugger--overlay
         'display
         (concat suggt* (buffer-substring beg-pt (+ beg-pt 1))))))))

(defun starhugger--active-overlay-p ()
  (and starhugger--overlay (overlay-buffer starhugger--overlay)))

(defun starhugger--init-overlay (suggt pt)
  "Initialize over to show SUGGT and mark PT as the original position."
  (when (starhugger--active-overlay-p)
    (delete-overlay starhugger--overlay))
  (when (<= pt (point-max))
    (setq starhugger--overlay
          (make-overlay pt (+ pt 1)
                        nil
                        ;; allow inserting before the overlay
                        t t))
    ;; (overlay-put starhugger--overlay 'keymap starhugger-at-suggestion-map)
    (overlay-put starhugger--overlay 'priority 1)
    (starhugger--update-overlay suggt pt)))

(defun starhugger-at-suggestion-beg-p (&optional cmd)
  "Return CMD (or true) when point is at suggestion start.
See `starhugger-inline-menu-item'."
  (and starhugger-inlining-mode
       starhugger--overlay
       (equal (overlay-start starhugger--overlay) (point))
       (or cmd t)))

(defun starhugger-inline-menu-item (cmd)
  "Return a CMD when only at the start of suggestion at run-time.
Use this when binding keys. See info node `(elisp) Extended Menu
Items'."
  `(menu-item "" ,cmd nil :filter starhugger-at-suggestion-beg-p))

(defun starhugger--suggestion-state (&optional pt)
  (if pt
      (save-excursion
        (goto-char pt)
        (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun starhugger--relevant-fetched-suggestions (&optional all pt)
  (-let* ((cur-state (starhugger--suggestion-state pt)))
    (-keep
     (-lambda ((state suggt)) (and (or all (equal cur-state state)) suggt))
     starhugger--suggestion-list)))

(defun starhugger--add-to-suggestion-list (suggestions state)
  (dolist (suggt suggestions)
    (when suggt
      (-let* ((elem (list state suggt)))
        (add-to-list 'starhugger--suggestion-list elem))))
  (-let* ((leng1 (length starhugger--suggestion-list)))
    (when (< starhugger-suggestion-list-size leng1)
      (setq starhugger--suggestion-list
            (ntake
             starhugger-suggestion-list-size starhugger--suggestion-list)))))

(defun starhugger--ensure-suggestion-list-syntax-highlighed (&optional force)
  "Ensure that fetched suggestions are syntax highlighted in current `major-mode'.
Normally only apply for unhighlighted suggestions, but FORCE
will (re-)apply for all."
  (-let* ((mjmode major-mode)
          (suggt-list starhugger--suggestion-list))
    (if (or force
            ;; some suggestions are not fontified?
            (-some
             (-lambda ((_state str)) (null (object-intervals str))) suggt-list))
        (-let* ((strings-around-alist
                 (save-excursion
                   (-->
                    suggt-list (-map (-lambda (([pt])) pt) it) (-uniq it)
                    (-map
                     (lambda (pt)
                       (goto-char pt)
                       (-let* (((toplv-beg . toplv-end)
                                (bounds-of-thing-at-point 'defun)))
                         (list
                          pt
                          (buffer-substring (or toplv-beg (point-min)) pt)
                          (buffer-substring pt (or toplv-end (point-max))))))
                     it))))
                (fontified-lst
                 (with-temp-buffer
                   (delay-mode-hooks
                     (funcall mjmode))
                   (-map
                    (-lambda ((state suggt))
                      (-let* ((fontified-str
                               (cond
                                ((and (not force) (object-intervals suggt))
                                 suggt)
                                (t
                                 (-let* (([pt _] state)
                                         ((pre suf)
                                          (alist-get pt strings-around-alist)))
                                   (erase-buffer)
                                   (insert pre)
                                   (-let* ((pt* (point)))
                                     (insert suggt)
                                     (insert suf)
                                     (font-lock-ensure)
                                     (buffer-substring
                                      pt* (+ pt* (length suggt)))))))))
                        (list state fontified-str)))
                    suggt-list))))
          (setq starhugger--suggestion-list fontified-lst))
      suggt-list)))

(defun starhugger--try-show-most-recent-suggestion ()
  (-let* ((pt (point))
          (cur-state (starhugger--suggestion-state))
          (recent-suggt
           (-some
            (-lambda ((state suggt)) (and (equal cur-state state) suggt))
            starhugger--suggestion-list)))
    (when recent-suggt
      (starhugger--ensure-inlininng-mode)
      (when starhugger-debug
        (starhugger--log #'starhugger--try-show-most-recent-suggestion
                         "at" pt ":" recent-suggt))
      (starhugger--init-overlay recent-suggt pt))
    recent-suggt))

(defcustom starhugger-trim-spaces-around-prompt t
  "Whether to trim spaces in the prompt.
Trim space before the prompt, and in fill mode, spaces after the
prompt."
  :type 'boolean)

(defun starhugger--no-fill-prompt ()
  (-let* ((pt-cur (point)))
    (-->
     (buffer-substring-no-properties
      (max (- pt-cur starhugger-max-prompt-length) (point-min)) pt-cur)
     (if starhugger-trim-spaces-around-prompt
         (string-trim-left it)
       it))))

(defun starhugger--instruct-unique-fill-placeholder (content)
  (named-let
      recur ((placeholder "<FILL>"))
    ;; Repeat <FILL-FILL-...> as needed
    (cond
     ((string-search placeholder content)
      (recur (concat "<FILL-" (substring placeholder 2))))
     (:else
      placeholder))))

(defvar starhugger--instruct-system-prompt-default
  "You are a code/text completion expert. Your task is to fill in missing code or text.
The input format uses <FILL> to mark where content should be inserted.

Requirements:
- Provide ONLY the replacement text/code for the <FILL> placeholder without any natural language explanations that aren't syntactic comments
- Do NOT include markdown formatting, code blocks, or any other wrapper; the provided markdown markers are just for clarify, don't include them in your answer
- Do NOT repeat any surrounding text
- Include comments in the respective programming language's syntax when needed, also if you want to add remarks write them as comments
- Ensure proper indentation and formatting of the surrounding code
- The fill may not always be composed of multiple lines, it may be just a part of a line
- If the fill is part of an uncompleted function, just try to fill within that function without extending to writing another function outside of it")

(cl-defun starhugger-instruct-make-messages-prompt-default (prefix suffix &optional other-context &key language &allow-other-keys)
  "Return an OpenAI-compatible /chat/completions \"messages\" parameter in lisp.
User prompt is constructed from PREFIX, SUFFIX and OTHER-CONTEXT.
LANGUAGE: a short string is used to annotate the task, for this
implementation it defaults to `mode-name'.  For compatibility, the
function accepts any keyword arguments that future versions may use."
  (-let*
      ((fill-placeholder
        (starhugger--instruct-unique-fill-placeholder
         (concat other-context prefix suffix)))
       (prj-root (starhugger--project-root))
       (curr-filename
        (cond
         ((and buffer-file-name prj-root)
          (file-relative-name (file-truename buffer-file-name)
                              (file-truename prj-root)))
         (buffer-file-name
          (file-name-nondirectory buffer-file-name))))
       (language (or language (starhugger--guess-language-id)))
       (system-prompt
        (-->
         starhugger--instruct-system-prompt-default
         (if (equal "<FILL>" fill-placeholder)
             it
           (string-replace "<FILL>" fill-placeholder it))))
       (user-prompt
        (-->
         "%s\n%s
```%s
%s
```\n
The replacement for <FILL> is:"
         (if (equal "<FILL>" fill-placeholder)
             it
           (string-replace "<FILL>" fill-placeholder it))
         (format it
                 (or other-context "")
                 (or curr-filename "")
                 (or language "")
                 (concat prefix fill-placeholder suffix))
         (string-trim it))))
    `[((role . "system") (content . ,system-prompt))
      ((role . "user") (content . ,user-prompt))]))

(defcustom starhugger-instruct-make-messages-prompt-function
  #'starhugger-instruct-make-messages-prompt-default
  "Function to construct \"messages\" when using an instruction-tuned model.
See `starhugger-instruct-make-messages-prompt-default' for compatible
arguments and return value."
  :type 'function)

(defun starhugger--prompt-make-components ()
  (if (and starhugger-fill-in-the-middle
           ;; don't use fill mode when at trailing newlines
           (not (looking-at-p "\n*\\'")))
      (-let* ((intend-suf-len
               (floor
                (* starhugger-max-prompt-length
                   starhugger-prompt-after-point-fraction)))
              (intend-pre-len (- starhugger-max-prompt-length intend-suf-len))
              (avail-pre (- (point) (point-min)))
              (avail-suf (- (point-max) (point)))
              ([pre-beg-pos suf-end-pos]
               (cond
                ((and (> avail-pre intend-pre-len) (< avail-suf intend-suf-len))
                 (vector
                  (- (point) (- starhugger-max-prompt-length avail-suf))
                  (point-max)))
                ((and (< avail-pre intend-pre-len) (> avail-suf intend-suf-len))
                 (vector
                  (point-min)
                  (+ (point) (- starhugger-max-prompt-length avail-pre))))
                (t
                 (vector
                  (- (point) intend-pre-len) (+ (point) intend-suf-len)))))
              ([pre-beg-pos suf-end-pos]
               (vector
                (max (point-min) pre-beg-pos) (min (point-max) suf-end-pos)))
              (suf-str
               (-->
                (buffer-substring-no-properties (point) suf-end-pos)
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-right it)
                  it)))
              (pre-str
               (-->
                (buffer-substring-no-properties pre-beg-pos (point))
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-left it)
                  it))))
        (vector pre-str suf-str))
    (vector (starhugger--no-fill-prompt) nil)))

(defun starhugger--async-prompt (callback)
  "CALLBACK is called with the constructed prompt."
  (-let* (([pre-code suf-code] (starhugger--prompt-make-components)))
    (cond
     ((member starhugger-fill-in-the-middle '(instruct))
      (-let* ((messages
               (funcall starhugger-instruct-make-messages-prompt-function
                        pre-code
                        suf-code)))
        (funcall callback
                 nil
                 :messages messages
                 :prefix pre-code
                 :suffix suf-code)))
     (:else
      (funcall callback pre-code :prefix pre-code :suffix suf-code)))))

(defun starhugger--get-from-num-or-list (num-or-list &optional idx)
  (cond
   ((null num-or-list)
    nil)
   ((numberp num-or-list)
    num-or-list)
   ((numberp idx)
    (elt num-or-list idx))
   (idx
    (elt num-or-list 1))
   (t
    (elt num-or-list 0))))

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key interact force-new num prompt-fn max-new-tokens backend callback)
  "Show AI-powered code suggestions as overlays.
When an inline suggestion is already showing, new suggestions
will be fetched, you can switch to them by calling
`starhugger-show-next-suggestion' after fetching finishes. NUM:
number of suggestions to fetch at once (actually sequentially,
the newly fetched ones are appended silently). FORCE-NEW: try to
fetch different responses. Non-nil INTERACT: show spinner.
CALLBACK: to be called with the list of generated suggestions,
and a variadic plist."
  (interactive (list :interact t :force-new starhugger-inlining-mode))
  (-let*
      ((num
        (or num
            (starhugger--get-from-num-or-list
             starhugger-numbers-of-suggestions-to-fetch
             interact)))
       (orig-buf (current-buffer))
       (pt0 (point))
       (state (starhugger--suggestion-state))
       (prompt-fn (or prompt-fn #'starhugger--async-prompt))
       (prompt-callback
        (lambda (prompt &rest args)
          (when (or (null prompt) (< 0 (length prompt)))
            (starhugger--ensure-inlininng-mode)
            (letrec
                ((func
                  (lambda (fetch-time)
                    (apply
                     #'starhugger--query-internal
                     prompt
                     (lambda (generated-texts &rest returned-args)
                       (when (and (buffer-live-p orig-buf)
                                  (not (plist-get returned-args :error)))
                         (with-current-buffer orig-buf
                           (-let* ((suggt-1st
                                    (-first-item generated-texts)))
                             (starhugger--add-to-suggestion-list
                              generated-texts state)
                             ;; only display when didn't move or interactive (in that
                             ;; case we are explicitly waiting)
                             (when (or interact (= pt0 (point)))
                               (when (= 1 fetch-time)
                                 (starhugger--ensure-inlininng-mode)
                                 (starhugger--init-overlay suggt-1st pt0))
                               (cond
                                ((and (< fetch-time num)
                                      (< (length generated-texts) num))
                                 (funcall func (+ fetch-time 1)))
                                ((not (starhugger--active-overlay-p))
                                 (starhugger--ensure-inlininng-mode 0)))))))
                       (when callback
                         (apply callback generated-texts returned-args)))
                     :max-new-tokens
                     (or max-new-tokens
                         (starhugger--get-from-num-or-list
                          starhugger-max-new-tokens
                          interact))
                     :num-return-sequences num
                     :force-new (or force-new (< 1 fetch-time))
                     :spin (or starhugger-debug interact)
                     :caller #'starhugger-trigger-suggestion
                     :backend
                     backend
                     args))))
              (funcall func 1))))))
    (funcall prompt-fn prompt-callback)))

(defun starhugger--triggger-suggestion-prefer-cache
    (in-buffer position &optional cache-only callback)
  (when (and (equal in-buffer (current-buffer)) (equal position (point)))
    (or (starhugger--try-show-most-recent-suggestion)
        (when (not cache-only)
          (starhugger--cancel-requests-in-buffer
           (current-buffer)
           nil
           (lambda (request-plist)
             (member
              (plist-get request-plist :caller)
              '(starhugger-trigger-suggestion))))

          ;; TODO: prevent spamming requests
          (when starhugger-debug
            (-let* ((requests
                     (gethash
                      (current-buffer) (starhugger--running-request-table))))
              (when (<= 1 (length requests))
                (starhugger--log
                 (format "`%s' Already running %d requests in %S!"
                         'starhugger--triggger-suggestion-prefer-cache
                         (length requests)
                         (buffer-name))))))

          (starhugger-trigger-suggestion
           :callback callback
           :num
           (starhugger--get-from-num-or-list
            starhugger-numbers-of-suggestions-to-fetch
            nil))))))

(defun starhugger--cancel-request (plist)
  (-let* (((&plist :cancel-fn cancel-fn :process process) plist))
    (when process
      (add-function :around
                    (process-sentinel process)
                    #'starhugger--without-notify-request-error--a))
    (dlet ((starhugger-notify-request-error nil))
      (cond
       (cancel-fn
        (funcall cancel-fn))
       (process
        (dlet ((kill-buffer-query-functions '()))
          (-let* ((proc-buf (process-buffer process)))
            (delete-process process)
            (when proc-buf
              (kill-buffer proc-buf)))))))))

(defun starhugger--cancel-requests-in-buffer
    (buf &optional plist-lst plist-pred)
  (-let* ((plist-lst
           (or plist-lst (gethash buf (starhugger--running-request-table) '())))
          (new-plist-lst
           (-remove
            (lambda (plist)
              (cond
               ((or (null plist-pred) (funcall plist-pred plist))
                (starhugger--cancel-request plist)
                t)
               (:else
                nil)))
            plist-lst)))
    (puthash buf new-plist-lst (starhugger--running-request-table))))

(defun starhugger-cancel-all-requests-globally ()
  "Terminate running requests in all buffers."
  (interactive)
  (maphash
   (lambda (buf plist-lst)
     (starhugger--cancel-requests-in-buffer buf plist-lst))
   (starhugger--running-request-table)))

(defun starhugger-dismiss-suggestion (&optional stop-fetching)
  "Clear current suggestion.
Non-nil STOP-FETCHING (interactively, true by default): also
cancel unfinished fetches."
  (interactive (list (not current-prefix-arg)))
  (when stop-fetching
    (starhugger--cancel-requests-in-buffer (current-buffer)))
  (starhugger-inlining-mode 0))

(defcustom starhugger-trigger-suggestion-after-accepting t
  "Whether to continue triggering suggestion after accepting."
  :type 'boolean)

(defun starhugger-turn-off-completion-in-region-mode ()
  "Use this when inserting parsed response.
To prevent key binding conflicts such as TAB."
  (completion-in-region-mode 0))

(defvar starhugger-post-insert-hook
  '(starhugger-turn-off-completion-in-region-mode)
  "Hook run after inserting the parsed response.")

(defun starhugger--accept-suggestion-partially (by &optional args)
  "Insert a part of active suggestion by the function BY.
Accept the part that is before the point after applying BY on
ARGS. Note that BY should be `major-mode' independent (being
executed in a temporary buffer)."
  (-when-let* ((pos (overlay-start starhugger--overlay)))
    (dlet ((starhugger--inline-inhibit-changing-overlay t))
      (goto-char pos)
      (-let* ((suggt (starhugger--current-overlay-suggestion))
              (text-to-insert
               (with-temp-buffer
                 (insert suggt)
                 (goto-char (point-min))
                 (apply by args)
                 (buffer-substring (point-min) (point)))))
        (insert text-to-insert)
        (if (equal suggt text-to-insert)
            (progn
              (starhugger-dismiss-suggestion)
              (and starhugger-trigger-suggestion-after-accepting
                   (starhugger-trigger-suggestion :interact t)))
          (progn
            (starhugger--update-overlay
             (string-remove-prefix text-to-insert suggt))))
        ;; maybe put parentheses balancer here?
        (run-hooks 'starhugger-post-insert-hook)
        text-to-insert))))

(defun starhugger-accept-suggestion ()
  "Insert the whole suggestion."
  (interactive)
  (starhugger--accept-suggestion-partially
   (lambda ()
     (goto-char (point-max))
     ;; don't end at an empty newline awkwardly
     (skip-chars-backward "\n"))))

(defun starhugger-accept-suggestion-by-character (n)
  "Insert N characters from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-char (list n)))
(defun starhugger-accept-suggestion-by-word (n)
  "Insert N words from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-word (list n)))
(defun starhugger-accept-suggestion-by-line (n)
  "Insert N lines from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-line (list n)))
(defun starhugger-accept-suggestion-by-paragraph (n)
  "Insert N paragraphs from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-paragraph (list n)))


(defun starhugger-undo-accept-suggestion-partially ()
  "Undo all partial acceptances and go back."
  (interactive)
  (-let* ((orig-point
           (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (str (buffer-substring orig-point (point))))
    (dlet ((starhugger--inline-inhibit-changing-overlay t))
      (delete-char (- (length str)))
      (starhugger--update-overlay
       (concat str (starhugger--current-overlay-suggestion))))))


(defun starhugger--get-prev-suggestion-index (delta suggestions)
  (-let* ((leng (length suggestions))
          (cur-idx
           (-elem-index (starhugger--current-overlay-suggestion) suggestions)))
    (-->
     ;; either `+': recent at first or `-': recent at last, depends of
     ;; `starhugger--suggestion-list''s ordering
     (+ cur-idx delta)
     ;; disable wrapping
     (min it (- leng 1)) (max it 0))))

(defun starhugger-show-prev-suggestion (delta)
  "Show the previous suggestion.
With prefix argument DELTA, show the suggestion that is DELTA away."
  (interactive "p")
  (-let* ((pt (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (suggestions (starhugger--relevant-fetched-suggestions nil pt))
          (prev-idx (starhugger--get-prev-suggestion-index delta suggestions))
          (suggt (elt suggestions prev-idx)))
    (starhugger--update-overlay suggt pt)))

(defun starhugger-show-next-suggestion (delta)
  "Show the next suggestion.
With prefix argument DELTA, show the suggestion that is DELTA away."
  (interactive "p")
  (starhugger-show-prev-suggestion (- delta)))

(defun starhugger-show-fetched-suggestions (&optional all)
  "Display fetched suggestions at point, or ALL positions.
Note that the number of suggestions are limited by
`starhugger-suggestion-list-size'."
  (interactive "P")
  (starhugger--ensure-suggestion-list-syntax-highlighed all)
  (-let* ((prompt-end-pt
           (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (bufname (format "*%s %s*" 'starhugger-suggestions (buffer-name)))
          (suggestions*
           (-keep
            (-lambda (([pt] suggt))
              (and (or all (= prompt-end-pt pt))
                   (save-excursion
                     (goto-char pt)
                     (concat (buffer-substring (pos-bol) pt) suggt))))
            starhugger--suggestion-list)))
    (pop-to-buffer bufname)
    (read-only-mode 0)
    (erase-buffer)
    (insert (string-join suggestions* "\n\n\n\n"))
    (read-only-mode 1)))

(defun starhugger-goto-suggestion ()
  "Go to the beginning of inline suggestion."
  (interactive)
  (goto-char (overlay-start starhugger--overlay)))


;;;; Auto-mode

(defcustom starhugger-auto-idle-time 0.5
  "Seconds to wait after typing, before fetching.
Note that the time taken to fetch isn' instantaneous, so we have
to wait more after this unless the suggestion(s) is already
cached, for the suggestion to appear."
  :type 'float)

(defcustom starhugger-auto-dismiss-when-move-out t
  "Whether to dismiss suggestion when moving point outside."
  :type 'boolean)

(defvar-local starhugger--auto-timer nil)

(defvar-local starhugger--auto-positional-state-old nil)

(defun starhugger--auto-positional-state (&rest other-info)
  (vector
   (point) (buffer-substring-no-properties (point-min) (point-max)) other-info))

(defun starhugger--auto-after-change-h (&optional beg end old-len)
  (when (and this-command (not starhugger--inline-inhibit-changing-overlay))
    (-let* ((tick-current (starhugger--auto-positional-state beg end old-len)))
      ;; Avoid spamming requests: do not activate another idle timer that have
      ;; exactly the same positional state as the previous one, whose request is
      ;; probably still running
      (when (not (equal starhugger--auto-positional-state-old tick-current))
        (setq starhugger--auto-positional-state-old tick-current)
        (when (timerp starhugger--auto-timer)
          (cancel-timer starhugger--auto-timer))
        (setq
         starhugger--auto-timer
         (run-with-idle-timer
          starhugger-auto-idle-time
          nil
          #'starhugger--triggger-suggestion-prefer-cache
          (current-buffer)
          (point)
          ;; don't fetch new when deleting
          (< 0 old-len)
          ;; When receiving suggestions, reset positional state so that
          ;; automatic trigger can happen later
          (lambda (&rest _)
            (setq starhugger--auto-positional-state-old nil))))))))

(defun starhugger--auto-post-command-h ()
  (when (and starhugger--overlay
             starhugger-inlining-mode
             starhugger-auto-dismiss-when-move-out
             (not starhugger--inline-inhibit-changing-overlay))
    (-let* ((beg
             (overlay-get
              starhugger--overlay 'starhugger-ovlp-original-position))
            (end (overlay-end starhugger--overlay)))
      (unless (and (numberp beg) (numberp end) (<= beg (point) end))
        (starhugger-dismiss-suggestion)))))

;;;###autoload
(define-minor-mode starhugger-auto-mode
  "Automatic `starhugger-trigger-suggestion' in current buffer."
  :lighter " 💫"
  :global nil
  (if starhugger-auto-mode
      (progn
        (add-hook 'post-command-hook #'starhugger--auto-post-command-h nil t)
        (add-hook 'after-change-functions #'starhugger--auto-after-change-h nil t))
    (progn
      (remove-hook 'post-command-hook #'starhugger--auto-post-command-h t)
      (remove-hook 'after-change-functions #'starhugger--auto-after-change-h t))))

;;;; Other commands

;;; starhugger.el ends here

(provide 'starhugger)

;; Local Variables:
;; byte-compile-docstring-max-column: 200
;; End:
