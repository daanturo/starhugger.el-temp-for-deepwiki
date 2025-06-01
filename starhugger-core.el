;;; starhugger-core.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Core library of starhugger.el


;;; Code:

(require 'cl-macs)

(require 'compat)
(require 'dash)

(require 'request)

(declare-function project-root "project")
(declare-function json-pretty-print-buffer "json")

;;;; Helpers

(defmacro starhugger--with-buffer-scrolling (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
Like `with-current-buffer', but allow scrolling the visible
window of BUFFER-OR-NAME when at the buffer end."
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

(defmacro with-live-buffer-or-current (buffer &rest body)
  "If BUFFER is live, execute BODY in it, else in current buffer."
  (declare (debug t) (indent defun))
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer
         ,@body)
     (progn
       ,@body)))

(defvar starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log requests and responses's data.")

(cl-defun starhugger--insert-log* (objs &key (end "\n") (sep " ") (fmt "%s") buffer)
  (-let* ((buf (or buffer starhugger-generated-buffer)))
    (when (not (get-buffer buf))
      (get-buffer-create buf)
      (with-current-buffer buf
        (setq-local outline-regexp "#+")
        (setq-local buffer-read-only t)))
    (starhugger--with-buffer-scrolling
      buf
      (dlet ((inhibit-read-only t))
        (goto-char (point-max))
        (while (not (seq-empty-p objs))
          (-let* ((obj (seq-first objs)))
            (setq objs (seq-rest objs))
            (insert (format fmt obj))
            (when (and sep (not (seq-empty-p objs)))
              (insert sep))))
        (insert end)))))

(defun starhugger--insert-log (&rest objs)
  (starhugger--insert-log* objs))

(defun starhugger--json-pretty-string (str)
  "Return JSON string STR prettified (formatted).
Return STR unchanged if any error(s) happen."
  (with-temp-buffer
    (condition-case _err
        (progn
          (insert str)
          (json-pretty-print-buffer)
          (buffer-substring-no-properties (point-min) (point-max)))
      (error str))))

(cl-defmacro starhugger--lambda (&rest body)
  "Anonymous function with BODY and Common Lisp argument list."
  (declare (debug t) (indent defun))
  `(cl-function (lambda ,@body)))

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (-->
   object (apply #'json-serialize it args) (encode-coding-string it 'utf-8)))

(defun starhugger--json-parse-buffer ()
  (json-parse-buffer :object-type 'alist))


;;;; Making requests

(defvar starhugger-debug nil)

(defvar starhugger-before-request-hook '()
  "Hook run before making an HTTP request.")

(defvar starhugger--running-request-table--table nil
  "Keys are buffers.")
(defun starhugger--running-request-table ()
  (unless starhugger--running-request-table--table
    (setq starhugger--running-request-table--table
          (make-hash-table :test #'equal)))
  starhugger--running-request-table--table)

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

(defvar starhugger-post-process-chain-default
  `(
    ;; Remove reasoning models's thinking tokens
    ("\\(\n?\\|^\\)<think>[^z-a]*?</think>[ \t\n\r]*" "")
    starhugger-post-process-instruct-markdown-code-block)
  "List of post-processing steps for model responses.
This is useful for instruction-tuned models, base models generally don't need
post-processing.
Each element can be:
- A variadic function whose arguments are (generated-text &rest _) and returns
  the processed text.
- A list of 2 strings: `replace-regexp-in-string''s first 2 arguments.")

(defvar starhugger-notify-request-error t
  "Whether to notify if an error was thrown when the request is completed.")

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

(defun starhugger--log-request-data (url data time-beg &optional time-end)
  (-let* ((time-fmt "%FT%T.%3N")
          (time-beg-str (format-time-string time-fmt time-beg))
          (time-end-str (format-time-string time-fmt time-end))
          (data-json
           (-->
            data
            (if (stringp it)
                it
              (json-serialize it))
            ;; TODO: measure the performance penalty of `json-pretty-print'
            (starhugger--json-pretty-string it)))
          (heading
           (-->
            (if time-end
                (format "\n# [%s] request, [%s] response:"
                        time-beg-str
                        time-end-str)
              (format "\n# [%s] request:" time-beg-str))
            (propertize it 'face '(:foreground "yellow" :weight bold)))))
    (starhugger--insert-log*
     (list heading (format "{\"URL\": %S}" url) data-json "\n")
     :sep "\n")))

(cl-defun starhugger--request-el-request (url &rest settings &key type data error complete parser &allow-other-keys)
  "Wrapper around (`request' URL @SETTINGS)."
  (declare (indent defun))
  (-let* ((time-beg (current-time))
          (data-in data)
          (_
           (progn
             (starhugger--log-request-data url data-in time-beg)))
          (complete-fn
           (starhugger--lambda (&rest result &key data &allow-other-keys)
             (-let* ((time-end (current-time))
                     (data-out data))
               (unwind-protect
                   (when complete
                     (apply complete result))
                 (starhugger--log-request-data url data-out time-beg
                                               time-end)))))
          (req-response
           (apply #'request
                  url
                  :type (or type "POST")
                  :parser
                  (or parser (lambda () (json-parse-buffer :object-type 'alist)))
                  :error (or error #'ignore)
                  :complete
                  complete-fn
                  settings))
          (req-buf (request-response--buffer req-response)))
    ;;  Silence errors when aborting, see https://github.com/tkf/emacs-request/issues/226.
    (add-function :around
                  (process-sentinel
                   (get-buffer-process (request-response--buffer req-response)))
                  #'starhugger--request-el--silent-sentinel-a)
    (list
     :request-response req-response
     :process (get-buffer-process req-buf)
     :cancel-fn (lambda () (request-abort req-response)))))

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

;;;;; Backends

(cl-defun starhugger--context-unimplemented (_prefix _suffix callback &rest _)
  (funcall callback nil))

(defclass starhugger-config ()
  ((url :initarg :url :type string :documentation "API endpoint.")
   (api-key
    :initarg :api-key
    :initform nil
    :type (or string null)
    :documentation "For authorization.")
   (request-type :initarg :request-type :initform "POST" :type string)
   (request-headers
    :initarg :request-headers
    :type list
    :documentation "Depends on API-KEY by default.")
   (request-parser
    :initarg :request-parser
    :initform #'starhugger--json-parse-buffer
    :type function
    :documentation "`request''s PARSER argument.")
   (parameters
    :initarg :parameters
    :initform '((stream . :false))
    :type list
    :documentation "Additional request parameters.")
   (model
    :initarg :model
    :type string
    :documentation "Unique model name on the platform.")
   (num
    :initarg :num
    :initform 1
    :type integer
    :documentation "Number of responses to fetch.")
   (code-length
    :initarg :code-length
    :initform 2048
    :type integer
    :documentation "Length of current buffer's taken code.")
   (suffix-fraction
    :initarg :suffix-fraction
    :initform 0.25
    :type float
    :documentation "How much the suffix should take in CODE-LENGTH.")
   (context-fn
    :initarg :context-fn
    :initform #'starhugger--context-unimplemented
    :documentation
    "Asynchronous function to query interfile context.
Arguments: prefix, suffix, a callback function (will be called with the context
string), the rest are ignored at the moment.")
   (system-prompts
    :initarg :system-prompts
    :type list
    :documentation "Array of system prompts")
   (join-prompts
    :initarg :join-prompts
    :initform nil
    :type (or null string)
    :documentation
    "For non-chat completions, join all non-suffix into a single prompt.
Use when the provider doesn't support the \"prompt\" argument as an
array.  If this is a non-nil string, join prompts into one using it as
the separator."))
  :abstract t)
;; Constructor
(cl-defmethod initialize-instance :after
  ((config starhugger-config) &rest _args &key &allow-other-keys)
  (when (not (slot-boundp config 'request-headers))
    (-let* ((api-key (slot-value config 'api-key)))
      (setf (slot-value config 'request-headers)
            `(("Content-Type" . "application/json")
              ,@(and api-key
                     `(("Authorization" . ,(format "Bearer %s" api-key)))))))))

;;;;;; Generic methods

(cl-defgeneric starhugger--response-data-to-choices (_config data)
  (list data))

(cl-defgeneric starhugger--post-process-do (_config str)
  str)

(cl-defun starhugger--make-prompt-prefix-suffix (config)
  (-let* ((code-len (slot-value config 'code-length))
          (suf-frac (slot-value config 'suffix-fraction)))
    (-let* ((intend-suffix-len (floor (* code-len suf-frac)))
            (intend-prefix-len (- code-len intend-suffix-len))
            (avail-prefix-len (- (point) (point-min)))
            (avail-suffix-len (- (point-max) (point)))
            ([beg-of-prefix end-of-suffix]
             (cond
              ((and (> avail-prefix-len intend-prefix-len)
                    (< avail-suffix-len intend-suffix-len))
               (vector (- (point) (- code-len avail-suffix-len)) (point-max)))
              ((and (< avail-prefix-len intend-prefix-len)
                    (> avail-suffix-len intend-suffix-len))
               (vector (point-min) (+ (point) (- code-len avail-prefix-len))))
              (t
               (vector
                (- (point) intend-prefix-len) (+ (point) intend-suffix-len)))))
            ([beg-of-prefix end-of-suffix]
             (vector
              (max (point-min) beg-of-prefix) (min (point-max) end-of-suffix)))
            (prefix-str
             (-->
              (buffer-substring-no-properties
               beg-of-prefix (point))
              (string-trim-left it "[\n\r]+")))
            (suffix-str
             (-->
              (buffer-substring-no-properties
               (point) end-of-suffix)
              (string-trim-right it "[\n\r]+"))))
      (vector prefix-str suffix-str))))

(cl-defun starhugger--make-prompt-components (config callback)
  "CALLBACK is called with :context :prefix :suffix."
  (-let* (([prefix suffix] (starhugger--make-prompt-prefix-suffix config)))
    (funcall (slot-value config 'context-fn)
             prefix suffix
             (lambda (context)
               (funcall callback
                        :context context
                        :prefix prefix
                        :suffix suffix)))))

(cl-defgeneric starhugger--request-make-input-data
    (_config &rest args &key _prompt _prefix _suffix &allow-other-keys)
  (starhugger--json-serialize '()))

(cl-defmethod starhugger--request-do
  ((config starhugger-config)
   callback
   &rest
   args
   &key
   prefix
   suffix
   context
   num
   &allow-other-keys)
  (-let* ((input-data
           (apply #'starhugger--request-make-input-data config args)))
    (starhugger--request-el-request
      (slot-value config 'url)
      :headers (slot-value config 'request-headers)
      :data input-data
      :complete
      (starhugger--lambda (&rest result &key data error-thrown &allow-other-keys)
        (-let* ((generated-lst
                 (if error-thrown
                     '()
                   (starhugger--response-data-to-choices config data))))
          (funcall callback
                   generated-lst
                   :model (slot-value config 'model)
                   :error
                   (and error-thrown
                        `((error-thrown ,error-thrown) (data ,data)))))))))

(cl-defmethod starhugger--query-internal
  ((config starhugger-config)
   callback
   &rest
   args
   &key
   caller
   num
   &allow-other-keys)
  "CALLBACK is called with generated content choices & variadic info."
  (run-hooks 'starhugger-before-request-hook)
  (-let* ((orig-buf (current-buffer))
          (request-record nil))
    (starhugger--make-prompt-components
     config
     (starhugger--lambda (&rest async-prompt-result &key _context prefix suffix &allow-other-keys)
       (-let* ((code-len (+ (length prefix) (length suffix)))
               (request-do-callback
                (starhugger--lambda (content-choices
                                     &rest request-do-result &key error &allow-other-keys)
                  (with-live-buffer-or-current orig-buf
                    (cl-callf
                        (lambda (lst) (delete request-record lst))
                        (gethash orig-buf (starhugger--running-request-table) '()))
                    (-let* ((err-str (format "%S" error))
                            (processed-content-choices
                             (--map (starhugger--post-process-do config it)
                                    content-choices)))
                      (when (and error starhugger-notify-request-error)
                        (message "`starhugger' response error: %s" err-str))
                      (apply callback
                             processed-content-choices
                             request-do-result)))))
               (request-obj
                (cond
                 ((< 0 code-len)
                  (apply #'starhugger--request-do
                         config
                         request-do-callback
                         :num
                         num
                         async-prompt-result))
                 (:else
                  (funcall request-do-callback '())))))
         (when (< 0 code-len)
           (setq request-record (append request-obj `(:caller ,caller)))
           (push request-record
                 (gethash orig-buf (starhugger--running-request-table) '())))
         request-record)))))

(defclass starhugger-config-base-model (starhugger-config) () :abstract t)

;;;;;; Ollama

(defclass starhugger-config-ollama-api-generate (starhugger-config-base-model)
  ((url :initform "http://localhost:11434/api/generate")))

(cl-defmethod starhugger--response-data-to-choices ((config starhugger-config-ollama-api-generate) data)
  (-some--> data (list (alist-get 'response it))))

;;;;;; OpenAI-compatible API

(defclass starhugger-config-openai-compat (starhugger-config) () :abstract t)

(defclass starhugger-config-openai-compat-base-completions (starhugger-config-openai-compat starhugger-config-base-model)
  ((url :initform "http://localhost:11434/v1/completions")))

(cl-defmethod starhugger--response-data-to-choices
  ((config starhugger-config-openai-compat-base-completions) data)
  (-some-->
      data
    (alist-get 'choices it)
    (seq-map (lambda (choice) (alist-get 'text choice)) it)))

(defvar starhugger-instruct-default-system-prompts
  '(
    "You are a code/text completion expert. Your task is to fill in missing code or text.
The input format uses <FILL> to mark where content should be inserted.

Requirements:
- Provide ONLY the replacement text/code for the <FILL> placeholder without any natural language explanations that aren't syntactic comments
- Do NOT include markdown formatting, code blocks, or any other wrapper; the provided markdown markers are just for clarify, don't include them in your answer
- Do NOT repeat any surrounding text
- Include comments in the respective programming language's syntax when needed, also if you want to add remarks write them as comments
- Ensure proper indentation and formatting of the surrounding code
- The fill may not always be composed of multiple lines, it may be just a part of a line
- If the fill is part of an uncompleted function, just try to fill within that function without extending to writing another function outside of it"))

(defun starhugger--instruct-unique-fill-placeholder (content)
  (named-let
      recur ((placeholder "<FILL>"))
    ;; Repeat <FILL-FILL-...> as needed
    (cond
     ((string-search placeholder content)
      (recur (concat "<FILL-" (substring placeholder 2))))
     (:else
      placeholder))))

(cl-defun starhugger-instruct-prompts-default (config prefix suffix &optional other-context &key language &allow-other-keys)
  (-let* ((fill-placeholder
           (starhugger--instruct-unique-fill-placeholder
            (concat other-context prefix suffix)))
          (uniquify-fill-placeholder-fn
           (if (equal "<FILL>" fill-placeholder)
               #'identity
             (lambda (str) (string-replace "<FILL>" fill-placeholder str))))
          (prj-root (starhugger--project-root))
          (curr-filename
           (cond
            ((and buffer-file-name prj-root)
             (file-relative-name (file-truename buffer-file-name)
                                 (file-truename prj-root)))
            (buffer-file-name
             (file-name-nondirectory buffer-file-name))))
          (language (or language (starhugger--guess-language-id)))
          (system-prompts
           (-map
            uniquify-fill-placeholder-fn (slot-value config 'system-prompts)))
          (user-prompt
           (-->
            "%s\n%s
```%s
%s
```\n
The replacement for <FILL> is:"
            (funcall uniquify-fill-placeholder-fn it)
            (format it
                    (or other-context "")
                    (or curr-filename "")
                    (or language "")
                    (concat prefix fill-placeholder suffix))
            (string-trim it))))
    `[,@(--map `((role . "system") (content . ,it)) system-prompts)
      ((role . "user") (content . ,user-prompt))]))

(defclass starhugger-config-instruct-model (starhugger-config)
  ((prompt-fn
    :initarg :prompt-fn
    :initform #'starhugger-instruct-prompts-default
    :type function
    :documentation
    "Function to construct the system and user prompts for the instruction-tuned model.")
   (post-process
    :initarg :post-process
    :initform starhugger-post-process-chain-default
    :documentation
    "List of post-processing steps for model responses.
Each element can be:
- A variadic function whose arguments are (generated-text &rest _) and returns
  the processed text.
- A list of 2 strings: `replace-regexp-in-string''s first 2 arguments."))
  :abstract t)
(cl-defmethod initialize-instance :after
  ((instance starhugger-config-instruct-model) &rest args)
  (when (not (slot-boundp instance 'system-prompts))
    (setf (slot-value instance 'system-prompts)
          starhugger-instruct-default-system-prompts)))


(defclass starhugger-config-openai-compat-chat-completions (starhugger-config-openai-compat starhugger-config-instruct-model)
  ((url :initform "http://localhost:11434/v1/chat/completions")))

(cl-defmethod starhugger--response-data-to-choices
  ((config starhugger-config-openai-compat-chat-completions) data)
  (-some-->
      data (alist-get 'choices it)
      (seq-map (lambda (choice) (map-nested-elt choice '(message content))) it)))

;;;;; Completion

(cl-defmethod starhugger--post-process-do ((config starhugger-config-instruct-model) str)
  (named-let
      recur ((retval str) (chain (slot-value config 'post-process)))
    (-let* ((op (car chain)))
      (cond
       ((seq-empty-p chain)
        retval)
       ((and (listp op) (= 2 (length op)) (-every #'stringp op))
        (recur
         (replace-regexp-in-string (nth 0 op) (nth 1 op) retval) (cdr chain)))
       ((functionp op)
        (recur (funcall op retval) (cdr chain)))))))



;;; starhugger-core.el ends here

(provide 'starhugger-core)
