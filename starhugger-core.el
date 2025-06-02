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

(cl-defmacro starhugger--lambda (&rest body)
  "Anonymous function with BODY and Common Lisp argument list."
  (declare (debug nil) (indent defun))
  `(cl-function (lambda ,@body)))

(defun starhugger--seq-first (seq)
  "Get first element of SEQ.
Simple wrapper around `seq-first' that prevents error on empty vectors."
  (ignore-error (args-out-of-range)
    (seq-first seq)))

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (-->
   object (apply #'json-serialize it args) (encode-coding-string it 'utf-8)))

(defun starhugger--json-parse-buffer ()
  "`json-parse-buffer', prefer alist."
  (json-parse-buffer :object-type 'alist))

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

(defun starhugger--project-root (&optional dir)
  "Return the root directory of DIR (defaults to current directory)'s project.
Return nil if not found."
  (-some--> (project-current nil dir) (project-root it)))

(defvar starhugger--guess-prog-language-name--cache
  (make-hash-table :test #'equal))
(defun starhugger--guess-prog-language-name ()
  "Guess current buffer's programming language name."
  (with-memoization (gethash
                     major-mode starhugger--guess-prog-language-name--cache)
    (cond
     ((stringp mode-name)
      (replace-regexp-in-string "[ \t]" "-" (downcase mode-name)))
     (:else
      (replace-regexp-in-string
       "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))))

(defun starhugger--filename-relative-to-project ()
  "Return current filename relatively to project root.
If buffer has no file, return nil.  If no project root is found, return
current filename without its directory."
  (-let* ((prj-root (starhugger--project-root)))
    (cond
     ((and buffer-file-name prj-root)
      (file-relative-name (file-truename buffer-file-name)
                          (file-truename prj-root)))
     (buffer-file-name
      (file-name-nondirectory buffer-file-name)))))

;;;; Logging

(defmacro starhugger--with-buffer-scrolling (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
Like `with-current-buffer', but allow scrolling the visible window of
BUFFER-OR-NAME when at the buffer end."
  ;; TODO: replace/remove this once figuring out how the \"*Message*\" buffer
  ;; can retain active cursor position even when inserting at the end of buffer.
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

(defmacro starhugger--with-live-buffer-or-current (buffer &rest body)
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
  "Insert OBJS into BUFFER, END at the end, SEP as separator, FMT for `format'."
  (-let* ((buf (or buffer starhugger-generated-buffer)))
    (when (not (get-buffer buf))
      (get-buffer-create buf)
      (with-current-buffer buf
        (setq-local outline-regexp "///+")
        (setq-local buffer-read-only t)))
    (starhugger--with-buffer-scrolling
      buf
      (dlet ((inhibit-read-only t))
        (goto-char (point-max))
        (while (not (seq-empty-p objs))
          (-let* ((obj (starhugger--seq-first objs)))
            (setq objs (seq-rest objs))
            (insert (format fmt obj))
            (when (and sep (not (seq-empty-p objs)))
              (insert sep))))
        (insert end)))))

(defun starhugger--insert-log (&rest objs)
  "Variadic `starhugger--insert-log' on OBJS."
  (starhugger--insert-log* objs))

(defun starhugger--log-request-data (url lisp-data time-beg &optional time-end)
  (-let* ((time-fmt "%FT%T.%3N")
          (time-beg-str (format-time-string time-fmt time-beg))
          (time-end-str (format-time-string time-fmt time-end))
          (data-printing
           (-->
            lisp-data `((URL . ,url) (data . ,it)) (json-serialize it)
            ;; TODO: measure the performance penalty of `json-pretty-print', if
            ;; great enough call out to an external formatter like "prettier
            ;; --parser json"
            (starhugger--json-pretty-string it)))
          (heading
           (-->
            (if time-end
                (format "\n/// [%s] request, [%s] response:"
                        time-beg-str
                        time-end-str)
              (format "\n/// [%s] request:" time-beg-str))
            (propertize it 'face '(:foreground "yellow" :weight bold)))))
    (starhugger--insert-log* (list heading data-printing "\n") :sep "\n")))

;;;; Making requests

(defvar starhugger-debug nil)

(defvar starhugger-before-request-hook '()
  "Hook run before making an HTTP request.")

(defun starhugger-post-process-instruct-markdown-code-block (text &rest _)
  "If a single Markdown code block is contained in TEXT, extract it.
Else return as-is."
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

(cl-defun starhugger--request-el-request (url &rest settings &key error &allow-other-keys)
  "Wrapper around (`request' URL @SETTINGS)."
  (declare (indent defun))
  (-let* ((req-response
           (apply #'request url :error (or error #'ignore) settings))
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


;;;;; Generic

;;;;;; Default function slots

(cl-defun starhugger--context-dummy (_filename _prefix _suffix callback &rest _)
  "Unimplemented reference, just call CALLBACK with nil instead of queried context."
  (apply callback nil '()))

(cl-defgeneric starhugger-make-prompt-parameters-default
    (_config
     &rest args &key prefix _suffix _context _filename &allow-other-keys))

;;;;;; Generic classes

;; Some slots have default values as [] (vector) to differentiate with nil

(defclass starhugger-config ()
  ((url :initarg :url :type string :documentation "API endpoint.")
   (api-key
    :initarg :api-key
    :initform nil
    :type (or string null)
    :documentation "For authorization.")
   (parameters
    :initarg :parameters
    :initform '((stream . :false))
    :type list
    :documentation "Additional request parameters.")
   (model
    :initarg :model
    :type string
    :documentation "Unique model name on the platform.
This must be set!")
   (num
    :initarg :num
    :initform 1
    :type integer
    :documentation "Targeted amount of choices to fetch.")
   (code-length
    :initarg :code-length
    :initform 8192
    :type integer
    :documentation "Length of current buffer's taken code, in characters (not tokens).")
   (suffix-fraction
    :initarg :suffix-fraction
    :initform 0.25
    :type float
    :documentation "How much the suffix should take in CODE-LENGTH.")
   (context-fn
    :initarg :context-fn
    :initform #'starhugger--context-dummy
    :documentation
    "Asynchronous function to query repository-wide, or other context.
Arguments: relative filename, prefix, suffix, a callback function (will be called
with the context string), the rest are ignored at the moment.")
   (system-prompts
    :initarg :system-prompts
    :initform []
    :type sequence
    :documentation "Sequence of system prompts.")
   (join-prompts
    :initarg :join-prompts
    :initform nil
    :type (or null string)
    :documentation
    "For non-chat completions, join all non-suffix into a single prompt.
Use when the provider doesn't support the \"prompt\" argument as an
array.  If this is a non-nil string, join prompts into one using it as
the separator.")
   (prompt-params-fn
    :initarg :prompt-params-fn
    :initform #'starhugger-make-prompt-parameters-default
    :type function
    :documentation
    "Function to construct the system and/or user prompt array.")
   (post-process
    :initarg :post-process
    :initform []
    :documentation
    "List of post-processing steps for model responses.
Each element can be:
- A variadic function whose arguments are (generated-text &rest _) and returns
  the processed text.
- A list of 2 strings: `replace-regexp-in-string''s first 2 arguments."))
  :abstract t)
;; Constructor
(cl-defmethod initialize-instance :after
  ((config starhugger-config) &rest _args &key &allow-other-keys)
  (cl-assert (slot-value config 'model)))

(defclass starhugger-config-base-type-model (starhugger-config) () :abstract t)

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

(defclass starhugger-config-instruct-type-model (starhugger-config) () :abstract t)
(cl-defmethod initialize-instance :after
  ((instance starhugger-config-instruct-type-model) &rest _args)
  (when (equal [] (slot-boundp instance 'system-prompts))
    (setf (slot-value instance 'system-prompts)
          starhugger-instruct-default-system-prompts))
  (when (equal [] (slot-value instance 'post-process))
    (setf (slot-value instance 'post-process)
          starhugger-post-process-chain-default)))

(defclass starhugger-config-json-type-request (starhugger-config) () :abstract t)

;;;;;; Generic methods

(cl-defgeneric starhugger--choices-from-response-data (_config data))

(cl-defgeneric starhugger--post-process-do (_config str)
  str)

(cl-defgeneric starhugger--request-make-input-data
    (_config &rest _args &key _prompt _prefix _suffix &allow-other-keys)
  (:documentation
   ".
If the PROMPT argument is non-nil, prefer it over constructing from
PREFIX, SUFFIX, CONTEXT, etc."))

(cl-defgeneric starhugger--perform-request
    (config
     callback &rest args &key prefix suffix context num &allow-other-keys))

;;;;;;; Default methods

(cl-defmethod starhugger--perform-request
  ((config starhugger-config-json-type-request)
   callback
   &rest
   args
   &key
   &allow-other-keys)
  (let* ((time-beg (current-time))
         (url (slot-value config 'url))
         (api-key (slot-value config 'api-key))
         (headers
          `(("Content-Type" . "application/json")
            ,@(and api-key `(("Authorization" . ,(format "Bearer %s" api-key))))))
         (data-in-lisp
          (apply #'starhugger--request-make-input-data config args))
         (data-in-str (starhugger--json-serialize data-in-lisp)))
    (starhugger--log-request-data url data-in-lisp time-beg)
    (starhugger--request-el-request
      url
      :type "POST"
      :headers headers
      :parser (lambda () (json-parse-buffer :object-type 'alist))
      :data data-in-str
      :complete
      (starhugger--lambda (&rest result &key data error-thrown &allow-other-keys)
        (-let* ((time-end (current-time))
                (data-out-lisp data)
                (content-choices
                 (if error-thrown
                     '()
                   (starhugger--choices-from-response-data config data-out-lisp))))
          (unwind-protect
              (funcall callback
                       content-choices
                       :error
                       (and error-thrown
                            `((error-thrown ,error-thrown)
                              (data-out-lisp ,data-out-lisp))))
            (starhugger--log-request-data url data-out-lisp time-beg
                                          time-end)))))))

(cl-defun starhugger--format-prefix-suffix-prompts-with-optional-context
    (&key always-format context prefix suffix language filename &allow-other-keys)
  (-let* ((format-flag
           (or always-format
               (and context (< 0 (length (string-trim context)))))))
    (cond
     ((not format-flag)
      `((prefix . ,prefix) (suffix . ,suffix)))
     (:else
      (-let* ((prefix-format
               "%s\n
%s
```%s
%s")
              (suffix-format
               "%s
```")
              (new-prefix
               (format prefix-format
                       (or context "")
                       (or filename "")
                       (or language "")
                       prefix))
              (new-suffix (format suffix-format suffix)))
        `((prefix . ,new-prefix) (suffix . ,new-suffix)))))))

;; For base models, not sure how to handle repo context, if any
(cl-defmethod starhugger-make-prompt-parameters-default
  ((config starhugger-config-base-type-model)
   &rest
   args
   &key
   prefix
   _suffix
   _context
   _language
   _filename
   &allow-other-keys)
  (-let* ((system-prompts (slot-value config 'system-prompts))
          ((&alist
            'prefix formatted-prefix-with-context 'suffix formatted-suffix)
           (apply
            #'starhugger--format-prefix-suffix-prompts-with-optional-context
            args))
          (join-sep (slot-value config 'join-prompts))
          (prefix-lst `(,@system-prompts ,formatted-prefix-with-context)))
    (cond
     ((and (< 1 (length prefix-lst)) (not join-sep))
      `((prompt . ,prefix-lst) (suffix . ,formatted-suffix)))
     ((and (< 1 (length prefix-lst)) join-sep)
      `((prompt . ,(string-join prefix-lst join-sep))
        (suffix . ,formatted-suffix)))
     (:else
      `((prompt . ,formatted-prefix-with-context)
        (suffix . ,formatted-suffix))))))

(cl-defmethod starhugger--post-process-do ((config starhugger-config) str)
  (named-let
      recur ((retval str) (chain (slot-value config 'post-process)))
    (-let* ((op (starhugger--seq-first chain)))
      (cond
       ((seq-empty-p chain)
        retval)
       ((and (listp op) (= 2 (length op)) (-every #'stringp op))
        (recur
         (replace-regexp-in-string (nth 0 op) (nth 1 op) retval) (seq-rest chain)))
       ((functionp op)
        (recur (funcall op retval) (seq-rest chain)))))))

;;;;; Backends

;;;;;; Ollama

(defclass starhugger-config-ollama-api-generate (starhugger-config-base-type-model starhugger-config-json-type-request)
  ((url :initform "http://localhost:11434/api/generate")
   (join-prompts
    :initform "\n"
    :documentation
    "Ollama only supports a non-array single string for the \"prompt\" parameter.
(Both \"/api/generate\" and \"/v1/completions\".)")))

(cl-defmethod starhugger--request-make-input-data
  ((config starhugger-config-ollama-api-generate)
   &rest
   args
   &key
   prompt
   _prefix
   suffix
   _num
   &allow-other-keys)
  `((model . ,(slot-value config 'model))
    ,@(cond
       (prompt
        `((prompt . ,prompt)))
       (:else
        (apply (slot-value config 'prompt-params-fn) config args)))
    ,@(and suffix `((suffix . ,suffix)))
    (options ,@(alist-get 'options (slot-value config 'parameters)))
    (stream . :false)
    ,@(slot-value config 'parameters)))

(cl-defmethod starhugger--choices-from-response-data ((_config starhugger-config-ollama-api-generate) data)
  (-some--> data (list (alist-get 'response it))))

;;;;;; OpenAI-compatible API

(defclass starhugger-config-openai-compat (starhugger-config-json-type-request) () :abstract t)

;;;;;;; OpenAI-compatible base completions

(defclass starhugger-config-openai-compat-base-completions (starhugger-config-openai-compat starhugger-config-base-type-model)
  ((url :initform "http://localhost:11434/v1/completions")))

(cl-defmethod starhugger--request-make-input-data
  ((config starhugger-config-openai-compat-base-completions)
   &rest
   args
   &key
   prompt
   _prefix
   suffix
   num
   &allow-other-keys)
  `((model . ,(slot-value config 'model))
    ,@(cond
       (prompt
        `((prompt . ,prompt)))
       (:else
        (apply (slot-value config 'prompt-params-fn) config args)))
    ,@(and suffix `((suffix . ,suffix)))
    (echo . :false)
    (stream . :false)
    (n . ,(or num (slot-value config 'num)))
    ,@(slot-value config 'parameters)))

(cl-defmethod starhugger--choices-from-response-data
  ((_config starhugger-config-openai-compat-base-completions) data)
  (-some-->
      data
    (alist-get 'choices it)
    (seq-map (lambda (choice) (alist-get 'text choice)) it)))

;;;;;;; OpenAI-compatible chat completions (instruct)

(defclass starhugger-config-openai-compat-chat-completions (starhugger-config-openai-compat starhugger-config-instruct-type-model)
  ((url :initform "http://localhost:11434/v1/chat/completions")))

(defun starhugger--instruct-unique-fill-placeholder (content)
  (named-let
      recur ((placeholder "<FILL>"))
    ;; Repeat <FILL-FILL-...> as needed
    (cond
     ((string-search placeholder content)
      (recur (concat "<FILL-" (substring placeholder 1))))
     (:else
      placeholder))))

(cl-defmethod starhugger-make-prompt-parameters-default
  ((config starhugger-config-openai-compat-chat-completions)
   &rest
   args
   &key
   prefix
   suffix
   context
   _language
   _filename
   &allow-other-keys)
  (-let* ((fill-placeholder-unique
           (starhugger--instruct-unique-fill-placeholder
            (concat context prefix suffix)))
          (uniquify-fill-placeholder-fn
           (if (equal "<FILL>" fill-placeholder-unique)
               #'identity
             (lambda (str)
               (string-replace "<FILL>" fill-placeholder-unique str))))
          (system-prompts
           (-map
            uniquify-fill-placeholder-fn (slot-value config 'system-prompts)))
          ((&alist
            'prefix formatted-prefix-with-context 'suffix formatted-suffix)
           (apply
            #'starhugger--format-prefix-suffix-prompts-with-optional-context
            :always-format t args))
          (user-prompt
           (-->
            "%s
The replacement for %s is:"
            (format it
                    (concat
                     formatted-prefix-with-context
                     fill-placeholder-unique
                     formatted-suffix)
                    fill-placeholder-unique)
            (string-trim it))))
    ;; OpenAI-compatible /chat/completions 's "messages" parameter
    `((messages .
                [,@(--map `((role . "system") (content . ,it)) system-prompts)
                 ((role . "user") (content . ,user-prompt))]))))

(cl-defmethod starhugger--request-make-input-data
  ((config starhugger-config-openai-compat-chat-completions)
   &rest
   args
   &key
   prompt
   prefix
   suffix
   context
   num
   &allow-other-keys)
  (-let* ((messages-params
           (cond
            ((or (stringp prompt)
                 (and (proper-list-p prompt) (not (seq-empty-p prompt))))
             `((messages .
                         [,@(--map `((role . "system") (content . ,it))
                                   (slot-value config 'system-prompts))
                          ((role . "user") (content . ,prompt))])))
            (:else
             (apply (slot-value config 'prompt-params-fn) config args)))))
    `((model . ,(slot-value config 'model))
      ,@messages-params
      (stream . :false)
      (n . ,(or num (slot-value config 'num)))
      ,@(slot-value config 'parameters))))

(cl-defmethod starhugger--choices-from-response-data
  ((_config starhugger-config-openai-compat-chat-completions) data)
  (-some-->
      data (alist-get 'choices it)
      (seq-map (lambda (choice) (map-nested-elt choice '(message content))) it)))

;;;; Querying functions

(cl-defgeneric starhugger-query
    (config
     prompt
     callback
     &rest
     args
     &key
     context
     prefix
     suffix
     num
     &allow-other-keys))

(cl-defmethod starhugger-query
  ((config starhugger-config)
   prompt
   callback
   &rest
   args
   &key
   context
   prefix
   suffix
   _num
   request-start-callback
   &allow-other-keys)
  (cl-assert (or prompt prefix suffix context))
  (-let* ((req
           (apply #'starhugger--perform-request
                  config
                  (starhugger--lambda (content-choices
                                       &rest perform-request-result &key _error &allow-other-keys)
                    (-let* ((processed-content-choices
                             (--map (starhugger--post-process-do config it)
                                    content-choices)))
                      (apply callback
                             processed-content-choices
                             perform-request-result)))
                  args)))
    (when request-start-callback
      (funcall request-start-callback req))
    req))

(cl-defun starhugger--prompt-prefix-suffix-from-buffer (config)
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

(cl-defun starhugger--prompt-components-from-buffer (config callback)
  "CALLBACK is called with :context :prefix :suffix."
  (-let* ((filename (starhugger--filename-relative-to-project))
          (language (starhugger--guess-prog-language-name))
          ([prefix suffix]
           (starhugger--prompt-prefix-suffix-from-buffer config)))
    (funcall (slot-value config 'context-fn)
             filename prefix suffix
             (lambda (context &rest _)
               (funcall callback
                        :context context
                        :filename filename
                        :language language
                        :prefix prefix
                        :suffix suffix)))))

(cl-defmethod starhugger-query
  ((config starhugger-config)
   (_prompt (eql 'buffer))
   callback
   &rest
   args
   &key
   num
   &allow-other-keys)
  "CALLBACK is called with generated content choices & variadic info."
  (run-hooks 'starhugger-before-request-hook)
  (-let* ((orig-buf (current-buffer)))
    ;; Asynchronously construct prompt components
    (starhugger--prompt-components-from-buffer
     config
     (starhugger--lambda (&rest
                          async-prompt-result
                          &key
                          context
                          filename
                          prefix
                          suffix
                          &allow-other-keys)
       (-let* ((code-len (+ (length prefix) (length suffix)))
               ;; This callback is ran when the request to the language model
               ;; returns
               (request-do-callback
                (starhugger--lambda (processed-content-choices
                                     &rest request-do-result &key error &allow-other-keys)
                  (starhugger--with-live-buffer-or-current orig-buf
                    (-let* ((err-str (format "%S" error)))
                      (when (and error starhugger-notify-request-error)
                        (message "`starhugger' response error: %s" err-str))
                      (apply callback
                             processed-content-choices
                             request-do-result)))))
               (request-obj
                (cond
                 ((< 0 code-len)
                  (apply #'starhugger-query
                         config
                         nil
                         request-do-callback
                         :context context
                         :filename filename
                         :prefix prefix
                         :suffix suffix
                         :num
                         num
                         async-prompt-result))
                 (:else
                  (funcall request-do-callback '())))))
         request-obj)))))

(cl-defun starhugger--query-until-number (config num callback &rest args &key query-kwargs &allow-other-keys)
  "Using CONFIG, keep querying until NUM of returned choices are achieved.
CALLBACK is called (potentially) multiple time, each time with the
returned choices returned from a request, number of accumulated
choices (including the just returned ones), and the variable rest are
keyword information.  When CALLBACK returns nil, target is reached, or
an error is returned, stop querying.  QUERY-FN: a querying function such
as `starhugger-query' or `starhugger-query-auto-prompt', QUERY-KWARGS:
variadic keyword arguments for QUERY-FN.  ARGS is variadic."
  (-let* ((target-num num))
    (letrec ((fetching-loop
              (lambda (this-time-num old-accumulated-num)
                (-let* ((query-fn-callback
                         (starhugger--lambda (content-choices
                                              &rest
                                              query-internal-result
                                              &key
                                              error
                                              &allow-other-keys)
                           (-let* ((just-got-num (length content-choices))
                                   (new-accumulated-num
                                    (+ old-accumulated-num just-got-num))
                                   (next-time-num
                                    (- target-num new-accumulated-num))
                                   (cb-result
                                    (apply callback
                                           content-choices
                                           new-accumulated-num
                                           query-internal-result)))
                             (when (not
                                    (or error
                                        (null cb-result)
                                        (<= next-time-num 0)))
                               (funcall fetching-loop next-time-num))))))
                  (apply #'starhugger-query
                         config
                         (plist-get query-kwargs :prompt)
                         query-fn-callback
                         :num
                         this-time-num
                         query-kwargs)))))
      (funcall fetching-loop target-num 0))))

;;; starhugger-core.el ends here

(provide 'starhugger-core)
