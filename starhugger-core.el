;;; starhugger-core.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Core library of starhugger.el


;;; Code:

(require 'cl-macs)
(require 'eieio)

(require 'compat)
(require 'dash)

(require 'plz)

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

(defun starhugger--json-parse-buffer (&rest args)
  "`json-parse-buffer', prefer alist."
  (apply #'json-parse-buffer :object-type 'alist args))

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

(defun starhugger--json-truthy (value)
  "Return whether VALUE is not null nor false.
nil is truthy since that's an empty object!"
  (not (member value (list :false :null))))

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

(cl-defun starhugger--log-request-data (url
                                        lisp-data
                                        time-beg
                                        &optional
                                        time-end
                                        &rest
                                        _
                                        &key
                                        stream-accumulation
                                        &allow-other-keys)
  (-let* ((time-fmt "%FT%T.%3N")
          (time-beg-str (format-time-string time-fmt time-beg))
          (time-end-str (format-time-string time-fmt time-end))
          (data-printing
           (-->
            lisp-data
            `((URL . ,url) (data . ,it) (stream-accumulation . ,stream-accumulation))
            (json-serialize it)
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

(defvar starhugger-notify-request-error t
  "Whether to notify if an error was thrown when the request is completed.")

(defun starhugger--without-notify-request-error--a (func &rest args)
  (dlet ((starhugger-notify-request-error nil))
    (apply func args)))

(defvar starhugger--running-request-table--table
  (make-hash-table :test #'equal))
(defun starhugger--running-request-table ()
  "Return a hash table of buffer - running requests.
Hash table key: buffer.  Hash table value: list of plists.

Each plist has at least those properties:
- :caller - the request' caller.
- :id - a unique symbol to identify the request, typically generated by
  `gensym'.
At least one of:
- :cancel-fn - function called with 0 arguments to terminate the process.
- :process - network process.
Optionally:
- :object - return value of the underlying library's requesting function call.

When performing a relevant request, add the plist to this table under
the corresponding buffer key so that starhugger can terminate them."
  starhugger--running-request-table--table)

(defun starhugger--request-table-record (obj buf)
  (push obj (gethash buf (starhugger--running-request-table))))

(defun starhugger--request-table-remove (obj-or-id buf)
  (cl-callf
      (lambda (obj-list)
        (cond
         ((symbolp obj-or-id)
          (cl-delete-if (-lambda ((&plist :id id)) (equal obj-or-id id)) obj-list))
         (:else
          (delete obj-or-id obj-list))))
      (gethash buf (starhugger--running-request-table) '())))

(defun starhugger--cancel-request (obj)
  (-let* (((&plist :cancel-fn cancel-fn :process process) obj))
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
              (kill-buffer proc-buf)))))
       (:else
        (message "`starhugger' no known way to cancel request: %S" obj))))))

;;;;; Generic

;;;;;; Default function slots

(cl-defun starhugger--context-dummy (callback &rest args &key _filename _prefix _suffix &allow-other-keys)
  "Unimplemented reference, call CALLBACK with nil instead of queried context.
ARGS: unused."
  (apply callback nil '()))

(cl-defgeneric starhugger-make-prompt-parameters-default
    (_config
     &rest args &key prefix _suffix _context _filename &allow-other-keys))

(cl-defun starhugger--prompt-prefix-suffix-from-buffer (code-length suffix-fraction)
  (-let* ((intend-suffix-len (floor (* code-length suffix-fraction)))
          (intend-prefix-len (- code-length intend-suffix-len))
          (avail-prefix-len (- (point) (point-min)))
          (avail-suffix-len (- (point-max) (point)))
          ([beg-of-prefix end-of-suffix]
           (cond
            ((and (> avail-prefix-len intend-prefix-len)
                  (< avail-suffix-len intend-suffix-len))
             (vector (- (point) (- code-length avail-suffix-len)) (point-max)))
            ((and (< avail-prefix-len intend-prefix-len)
                  (> avail-suffix-len intend-suffix-len))
             (vector (point-min) (+ (point) (- code-length avail-prefix-len))))
            (t
             (vector
              (- (point) intend-prefix-len) (+ (point) intend-suffix-len)))))
          ([beg-of-prefix end-of-suffix]
           (vector
            (max (point-min) beg-of-prefix) (min (point-max) end-of-suffix)))
          (prefix-str
           (-->
            (buffer-substring-no-properties beg-of-prefix (point))
            (string-trim-left it "[\n\r]+")))
          (suffix-str
           (-->
            (buffer-substring-no-properties (point) end-of-suffix)
            (string-trim-right it "[\n\r]+"))))
    (vector prefix-str suffix-str)))

;;;;;; Generic classes

;; Some slots have default values as [] (vector) to differentiate with nil

(defclass starhugger-config ()
  ((model
    :initarg :model
    :type string
    :documentation "Mandatory unique model name on the platform.")
   (url :initarg :url :type string :documentation "API endpoint.")
   (api-key
    :initarg :api-key
    :initform nil
    :type (or string null)
    :documentation "For authorization.")
   (parameters
    :initarg :parameters
    :initform '((stream . :false))
    :type list
    :documentation
    "Additional request parameters.
\"stream\" being false is just an example, it's always enforced when completing.")
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
    :documentation
    "Sequence of system prompts.
Each element can be:
- A string: a system prompt.
- t : substitute with `starhugger-instruct-default-system-prompts'.

By default for chat models, this  will be initialized to [t].")
   (other-prompts
    :initarg :other-prompts
    :initform []
    :type sequence
    :documentation
    "Sequence of messages before the final user prompt, after system prompts.
Each element is an alist, each alist contains at least 2 keys: role and
content.  See
https://platform.openai.com/docs/api-reference/chat/create#chat-create-messages.
This is the more flexible version of the 'system-prompts slot, but still
only (singular) string are supported.

Example:

:other-prompts
[((role . \"user\")
  (content . \"(define (odd? n) (<FILL>))\"))
 ((role . \"assistant\")
  (content . \"not (= 0 (% n 2))\"))
 ((role . \"user\")
  (content . \";; Predicate function to check if number is non-negative\n<FILL>\"))
 ((role . \"assistant\")
  (content . \"(define (non-negative? n) (<= 0 n))\"))]

Currently this is only used for instruction-tuned models.")
   (join-prompts
    :initarg :join-prompts
    :initform nil
    :type (or null string)
    :documentation
    "For non-chat completions, join all non-suffix (prefix/prompt, system
prompts, etc.) into a single prompt using this separator.  Use when the
provider doesn't support the \"prompt\" argument as an array, such as
Ollama.  By default without repo-wide context, this isn't so relevant
since FIM models don't need system prompts nor instructions.")
   (prompt-params-fn
    :initarg :prompt-params-fn
    :initform #'starhugger-make-prompt-parameters-default
    :type function
    :documentation "Function to construct the system and/or user prompt parameters.")
   (stream
    :initarg :stream
    :initform nil
    :type boolean
    :documentation "Enable streaming.  When making requests, `starhugger' will limit n to 1
if streaming is enabled.  Post-processing won't be applied to the
incomplete content.")
   (post-process
    :initarg :post-process
    :initform []
    :documentation
    "Sequence of post-processing steps for model responses.
This is useful for instruction-tuned models, base models generally don't need
post-processing.
Each element can be:
- A variadic function whose argument list is (generated-text &rest _) and
  returns the processed text.
- A list of 2 strings: `replace-regexp-in-string''s first 2 arguments.
- t : substitute with `starhugger-post-process-default-chain'.

By default for chat models, this will be initialized to [t]."))
  :abstract t)
;; Constructor
(cl-defmethod initialize-instance :after
  ((config starhugger-config) &rest _args &key &allow-other-keys)
  (cl-assert (slot-value config 'model)))

(defclass starhugger-config-base-type-model (starhugger-config) () :abstract t)

(defvar starhugger-instruct-default-system-prompts
  '(
    "You are a code/text completion expert. Your task is to fill in missing code or text.
The input format uses <FILL> to mark where content should be inserted."
    ;; This may hurt completing in real Markdown files
    "Requirements:
- Provide ONLY the replacement text/code for the <FILL> placeholder without any natural language explanations that aren't syntactic comments.
- Answer in the programming language of the provided code, not automatically defaulting to Markdown with code blocks.
- Do NOT include Markdown formatting markers (```...```), code blocks, or any other wrappers.
- The provided Markdown markers are just for clarify, do NOT include them in your answer.
- Include comments in the respective programming language's syntax when needed.
- Do NOT repeat any surrounding text, nor make any changes to them.
- If you want to add remarks about fixes or improvements, write them as syntactic comments inside <FILL> instead of modifying.
- Ensure proper indentation and formatting of the surrounding code.
- The fill may not always be composed of multiple lines, it may be just a part of a line.
- If the fill is part of an uncompleted function, just try to fill within that function without extending to writing another function outside of it.
")
  "Default system prompts for instruct-tuned models.")

(defvar starhugger-post-process-default-chain
  `(
    ;; Remove reasoning models's thinking tokens
    ("\\(\n?\\|^\\)<think>[^z-a]*?</think>[ \t\n\r]*" "")
    starhugger-post-process-instruct-markdown-code-block)
  "List of post-processing steps for model responses.
See symbol `starhugger-config''s post-process attribute.")

(defclass starhugger-config-instruct-type-model (starhugger-config)
  ;; Cannot have a different :initarg from parent
  ((post-process :initarg :post-process :initform '(t))
   (role-system
    :initarg :role-system
    :initform "system"
    :documentation
    "For instruction-tuned models's \"message\" parameter's system prompts,
value of the field \"role\".  For some models the value is
\"developer\".")
   (role-user :initarg :role-user :initform "user"))
  (:default-initargs :post-process '(t))
  :abstract t)
(cl-defmethod initialize-instance :after
  ((instance starhugger-config-instruct-type-model) &rest _args)
  (when (equal [] (slot-value instance 'system-prompts))
    (setf (slot-value instance 'system-prompts) [t]))
  (when (equal [] (slot-value instance 'post-process))
    (setf (slot-value instance 'post-process) [t])))

(defclass starhugger-config-json-type-request (starhugger-config) () :abstract t)

(cl-defgeneric starhugger-get (obj slot &rest _args)
  (:documentation "Getter to get SLOT from OBJ.
Compared to an accessor/reader, this doesn't pollute the symbol list
with many more functions.")
  (slot-value obj slot))

(cl-defmethod starhugger-get ((obj starhugger-config) (_slot (eql 'system-prompts)))
  (-splice-list
   (lambda (it) (equal t it))
   starhugger-instruct-default-system-prompts
   (seq-into (slot-value obj 'system-prompts) 'list)))
(cl-defmethod starhugger-get ((obj starhugger-config) (_slot (eql 'post-process)))
  (-splice-list
   (lambda (it) (equal t it))
   starhugger-post-process-default-chain
   (seq-into (slot-value obj 'post-process) 'list)))

;;;;;; Generic methods

(cl-defgeneric starhugger--choices-from-response-data (_config data))

(cl-defgeneric starhugger--post-process-do (_config str)
  str)

(cl-defgeneric starhugger--request-make-input-data
    (config &rest args &key prompt prefix suffix &allow-other-keys)
  (:documentation
   "Construct the request's sending data, in lisp.
If the PROMPT argument is non-nil, prefer it over constructing from
PREFIX, SUFFIX, CONTEXT, etc."))

(cl-defgeneric starhugger--prompt-components-from-buffer
    (_config
     callback
     &rest
     args
     &key
     context-fn
     code-length
     suffix-fraction
     &allow-other-keys)
  (:documentation "CALLBACK is called with :context :prefix :suffix.")
  (-let* ((filename (starhugger--filename-relative-to-project))
          (language (starhugger--guess-prog-language-name))
          ([prefix suffix]
           (starhugger--prompt-prefix-suffix-from-buffer
            code-length suffix-fraction))
          (plist
           (list
            :filename filename
            :language language
            :prefix prefix
            :suffix suffix))
          (context-callback
           (starhugger--lambda
             (context &rest _) (apply callback :context context plist))))
    (cond
     (context-fn
      (apply context-fn context-callback plist))
     (:else
      (apply context-callback nil plist)))))

(cl-defgeneric starhugger--perform-request
    (config
     callback &rest args &key prefix suffix context num &allow-other-keys))

(defvar-local starhugger--stream-buffer-headers-end-position nil
  "The found end of headers in the network process buffer.")
(defvar-local starhugger--stream-buffer-current-position nil
  "Last successfully parsed position.
Save just to sure something else moves the point in the buffer after
parsing.")
(defvar-local starhugger--stream-buffer-accumulation nil
  "Accumulated content of the response.  A string.
`starhugger''s streaming mode doesn't support multiple responses in a
single request, since handling multiple accumulated answers with
different lengths is complex, and many providers don't support n > 1
anyway.")

(cl-defgeneric starhugger--stream-parse-chunk-to-content (config &rest _)
  (:documentation "In the network process buffer's particular position, parse and return an
association list with at least 2 keys: - content : a single answer as
string, part of the complete response.  - done : Whether if this is the
last chunk.

If parsing is successful, move point past the parsed object and return,
else signal an error and don't move point, as the chunk may not be a
complete object."))

(cl-defun starhugger--stream-make-filter (config stream-callback final-callback &rest _ &key &allow-other-keys)
  "Return a function for `plz''s :filter, that wraps STREAM-CALLBACK.
CONFIG is used to dispatch the parser."
  (lambda (process chunk)
    (with-current-buffer (process-buffer process)
      (save-excursion (internal-default-process-filter process chunk))
      (when (and (null starhugger--stream-buffer-headers-end-position))
        (goto-char (point-min))
        (re-search-forward plz-http-end-of-headers-regexp)
        (setq starhugger--stream-buffer-headers-end-position (point)))
      (if starhugger--stream-buffer-current-position
          (goto-char starhugger--stream-buffer-current-position)
        (setq starhugger--stream-buffer-current-position
              starhugger--stream-buffer-headers-end-position))
      (-let* ((parse-success t)
              (done-final nil)
              (last-success-parsed nil))
        ;; Potentially multiple complete and incomplete objects in an outputting
        ;; chunk
        (while parse-success
          (condition-case _
              (-let* ((parsed-alist
                       (starhugger--stream-parse-chunk-to-content config))
                      ((&alist 'content new-content 'done done-json-val)
                       parsed-alist))
                (setq starhugger--stream-buffer-accumulation
                      (concat
                       starhugger--stream-buffer-accumulation new-content))
                (setq last-success-parsed parsed-alist)
                (setq done-final
                      (or done-final (starhugger--json-truthy done-json-val))))
            ((error json-parse-error) (setq parse-success nil))))
        (setq starhugger--stream-buffer-current-position (point))
        (funcall stream-callback
                 starhugger--stream-buffer-accumulation
                 :done done-final)
        (when done-final
          (funcall
           final-callback
           last-success-parsed
           nil
           :stream-accumulation starhugger--stream-buffer-accumulation))))))

;;;;;;; Default methods

(cl-defun starhugger--stream-chunk-to-content-choices--parse-data:-json ()
  "Parse this format into Lisp object: \"data: {...\".
Or the JSON object at line start."
  (re-search-forward
   (concat
    "^\\(?:[[:alnum:]_-]+[ \t]*:[ \t]*\\)?"
    "\\({.*\\|\\[[ \t]*DONE[ \t]*\\]\\)"))
  (cond
   ((string-match "^[ \t]*\\[[ \t]*DONE[ \t]*\\][ \t]*$" (match-string 1))
    `((done . t)))
   (:else
    (goto-char (match-beginning 1))
    (starhugger--json-parse-buffer))))


(cl-defmethod starhugger--perform-request
  ((config starhugger-config-json-type-request)
   callback
   &rest
   args
   &key
   caller
   stream-callback
   &allow-other-keys)
  (run-hooks 'starhugger-before-request-hook)
  (let* ((time-beg (current-time))
         (url (slot-value config 'url))
         (api-key (slot-value config 'api-key))
         (headers
          `(("Content-Type" . "application/json")
            ,@(and api-key `(("Authorization" . ,(format "Bearer %s" api-key))))))
         (data-in-lisp
          (apply #'starhugger--request-make-input-data config args))
         (data-in-str (starhugger--json-serialize data-in-lisp))
         (_ (starhugger--log-request-data url data-in-lisp time-beg))
         (stream-flag (starhugger-get config 'stream))
         (complete-fn
          (starhugger--lambda (data-alist
                               &optional error-thrown &key stream-accumulation &allow-other-keys)
            (-let* ((time-end (current-time))
                    (content-choices
                     (cond
                      (error-thrown
                       '())
                      (stream-accumulation
                       (ensure-list stream-accumulation))
                      (t
                       (starhugger--choices-from-response-data
                        config data-alist)))))
              (unwind-protect
                  (funcall callback
                           content-choices
                           :error
                           (and error-thrown
                                `((error-thrown ,error-thrown)
                                  (data ,data-alist))))
                (starhugger--log-request-data
                 url
                 data-alist
                 time-beg
                 time-end
                 :stream-accumulation stream-accumulation)))))
         (filter-fn
          (and stream-flag
               (starhugger--stream-make-filter
                config stream-callback complete-fn))))
    (letrec ((plz-process
              (apply #'plz
                     'post
                     url
                     :headers headers
                     :as
                     (lambda ()
                       (cond
                        (stream-flag
                         (ignore-errors
                           (starhugger--json-parse-buffer)))
                        (:else
                         (starhugger--json-parse-buffer))))
                     :body data-in-str
                     :then
                     (lambda (data-alist &rest _)
                       (funcall complete-fn data-alist))
                     :else
                     (lambda (err &rest _) (funcall complete-fn '() err))
                     (append (and stream-flag (list :filter filter-fn))))))
      (-let* ()
        (list :process plz-process :object plz-process :caller caller)))))

(cl-defmethod starhugger--prompt-components-from-buffer
  ((config starhugger-config) callback &rest args &key &allow-other-keys)
  (apply #'cl-call-next-method
         config
         callback
         :context-fn (starhugger-get config 'context-fn)
         :code-length (starhugger-get config 'code-length)
         :suffix-fraction
         (starhugger-get config 'suffix-fraction)
         args))

(cl-defun starhugger--format-prefix-suffix-prompts-with-optional-context (&key always-format context prefix suffix language filename &allow-other-keys)
  (-let* ((prefix (or prefix ""))
          (suffix (or suffix ""))
          (format-flag
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

(cl-defmethod starhugger-make-prompt-parameters-default
  ((config starhugger-config-base-type-model)
   &rest
   args
   &key
   _prefix
   _suffix
   _context
   _language
   _filename
   &allow-other-keys)
  (-let* ((system-prompts (starhugger-get config 'system-prompts))
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
      recur
      ((retval str)
       (chain
        (append
         '(
           ;; Carriage returns
           ("\r" ""))
         (starhugger-get config 'post-process))))
    (-let* ((op (starhugger--seq-first chain)))
      (cond
       ((seq-empty-p chain)
        retval)
       ((and (listp op) (= 2 (length op)) (-every #'stringp op))
        (recur
         (replace-regexp-in-string (nth 0 op) (nth 1 op) retval)
         (seq-rest chain)))
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
  `((model . ,(starhugger-get config 'model))
    (stream . ,(or (starhugger-get config 'stream) :false))
    ,@(cond
       (prompt
        `((prompt . ,prompt)))
       (:else
        (apply (starhugger-get config 'prompt-params-fn) config args)))
    ,@(and suffix `((suffix . ,suffix)))
    (options ,@(alist-get 'options (starhugger-get config 'parameters)))
    ,@(starhugger-get config 'parameters)))

(cl-defmethod starhugger--choices-from-response-data ((_config starhugger-config-ollama-api-generate) data)
  (-some--> data (list (alist-get 'response it))))

(cl-defmethod starhugger--stream-parse-chunk-to-content
  ((_config starhugger-config-ollama-api-generate) &rest _)
  (-let* ((alist
           (starhugger--stream-chunk-to-content-choices--parse-data:-json)))
    `((content . ,(map-nested-elt alist '(response)))
      (done . ,(map-nested-elt alist '(done)))
      ,@alist)))

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
  `((model . ,(starhugger-get config 'model))
    (echo . :false)
    (stream . ,(or (starhugger-get config 'stream) :false))
    (n .
       ,(if (starhugger-get config 'stream)
            1
          (or num (starhugger-get config 'num))))
    ,@(cond
       (prompt
        `((prompt . ,prompt)))
       (:else
        (apply (starhugger-get config 'prompt-params-fn) config args)))
    ,@(and suffix `((suffix . ,suffix)))
    ,@(starhugger-get config 'parameters)))

(cl-defmethod starhugger--choices-from-response-data
  ((_config starhugger-config-openai-compat-base-completions) data)
  (-some-->
      data
    (alist-get 'choices it)
    (seq-map (lambda (choice) (alist-get 'text choice)) it)))

(cl-defmethod starhugger--stream-parse-chunk-to-content
  ((_config starhugger-config-openai-compat-base-completions) &rest _)
  (-let* ((alist
           (starhugger--stream-chunk-to-content-choices--parse-data:-json)))
    `((content . ,(map-nested-elt alist '(choices 0 text)))
      (done . ,(map-nested-elt alist '(choices 0 finish_reason)))
      ,@alist)))

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
  ((config starhugger-config-instruct-type-model)
   &rest
   args
   &key
   prefix
   suffix
   context
   _language
   _filename
   &allow-other-keys)
  (-let* ((system-prompts (starhugger-get config 'system-prompts))
          (fill-placeholder-unique
           (starhugger--instruct-unique-fill-placeholder
            (concat context prefix suffix)))
          (uniquify-<FILL>
           (if (equal "<FILL>" fill-placeholder-unique)
               #'identity
             (lambda (str)
               ;; Only support simple string content, leave others such as array
               (if (stringp str)
                   (string-replace "<FILL>" fill-placeholder-unique str)
                 str))))
          (system-prompts* (-map uniquify-<FILL> system-prompts))
          (other-prompts*
           (-map
            (lambda (alist)
              (setf (alist-get 'content alist)
                    (funcall uniquify-<FILL> (alist-get 'content alist)))
              alist)
            (starhugger-get config 'other-prompts)))
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
                [,@(--map `((role . ,(starhugger-get config 'role-system))
                            (content . ,it))
                          system-prompts*)
                 ,@other-prompts*
                 ((role . ,(starhugger-get config 'role-user))
                  (content . ,user-prompt))]))))

(cl-defmethod starhugger--request-make-input-data
  ((config starhugger-config-openai-compat-chat-completions)
   &rest
   args
   &key
   prompt
   _prefix
   _suffix
   _context
   num
   &allow-other-keys)
  (-let* ((messages-params
           (cond
            ((or (stringp prompt)
                 (and (proper-list-p prompt) (not (seq-empty-p prompt))))
             `((messages .
                         [,@(--map `((role . "system") (content . ,it))
                                   (starhugger-get config 'system-prompts))
                          ((role . "user") (content . ,prompt))])))
            (:else
             (apply (starhugger-get config 'prompt-params-fn) config args)))))
    `((model . ,(starhugger-get config 'model))
      (stream . ,(or (starhugger-get config 'stream) :false))
      (n .
         ,(if (starhugger-get config 'stream)
              1
            (or num (starhugger-get config 'num))))
      ,@messages-params ;
      ,@(starhugger-get config 'parameters))))

(cl-defmethod starhugger--choices-from-response-data
  ((_config starhugger-config-openai-compat-chat-completions) data)
  (-some-->
      data (alist-get 'choices it)
      (seq-map (lambda (choice) (map-nested-elt choice '(message content))) it)))

(cl-defmethod starhugger--stream-parse-chunk-to-content
  ((_config starhugger-config-openai-compat-chat-completions) &rest _)
  (-let* ((alist
           (starhugger--stream-chunk-to-content-choices--parse-data:-json)))
    `((content . ,(map-nested-elt alist '(choices 0 delta content)))
      (done . ,(map-nested-elt alist '(choices 0 finish_reason)))
      ,@alist)))

;;;; Querying functions

(cl-defun starhugger-query-helper (config
                                   _callback-after-response
                                   callback-after-prompts
                                   &rest
                                   kwargs
                                   &key
                                   prompt
                                   caller
                                   buffer
                                   prefix
                                   suffix
                                   &allow-other-keys)
  "Wrapper to simply writing an implementation for `starhugger-query'."
  (-let* ((id (gensym "starhugger"))
          (record-cancel-fn
           (lambda (cancel-info)
             (-let* ((cancel-obj
                      (cond
                       ((functionp cancel-info)
                        (list :id id :caller caller :cancel-fn cancel-info))
                       ((plistp cancel-info)
                        (append (list :id id :caller caller) cancel-info)))))
               (cl-assert cancel-obj)
               (starhugger--request-table-record cancel-obj buffer)
               ;; This may do unexpected things if callback-after-response is a
               ;; named function?
               ;; (letrec ((clear-adv
               ;;           (lambda (&rest _)
               ;;             (remove-function callback-after-response clear-adv)
               ;;             (starhugger--request-table-remove id buffer))))
               ;;   (add-function :before callback-after-response clear-adv))
               )))
          (remove-cancel-fn
           (lambda () (starhugger--request-table-remove id buffer)))
          (callback
           (starhugger--lambda (&rest args1)
             (apply callback-after-prompts
                    :record-cancel record-cancel-fn
                    :remove-cancel
                    remove-cancel-fn
                    args1))))
    (cond
     ;; No need to find prompt components again
     ((or prompt prefix suffix)
      (apply callback kwargs))
     (:else
      (apply #'starhugger--prompt-components-from-buffer
             config
             callback
             kwargs)))))

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
     &allow-other-keys)
  (:documentation
   "CALLBACK is called with generated content choices & variadic info.
Including :error."))

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
   buffer
   caller
   _num
   &allow-other-keys)
  (cl-assert (or prompt prefix suffix context))
  (-let* ((buffer (or buffer (current-buffer))))
    (apply #'starhugger-query-helper
           config callback
           (starhugger--lambda (&rest
                                helper-args &key record-cancel remove-cancel &allow-other-keys)
             (-let* ((req-plist
                      (apply #'starhugger--perform-request
                             config
                             (starhugger--lambda (content-choices
                                                  &rest
                                                  perform-request-result
                                                  &key
                                                  _error
                                                  &allow-other-keys)
                               (funcall remove-cancel)
                               (-let* ((processed-content-choices
                                        (--map (starhugger--post-process-do config it)
                                               content-choices)))
                                 (apply callback
                                        processed-content-choices
                                        perform-request-result)))
                             :caller
                             (or caller #'starhugger-query)
                             helper-args)))
               (funcall record-cancel req-plist)
               req-plist))
           :buffer buffer
           :prompt prompt args)))

(cl-defmethod starhugger-query
  ((config starhugger-config)
   (_prompt (eql 'buffer))
   callback
   &rest
   args
   &key
   num
   buffer
   &allow-other-keys)
  (-let* ((orig-buf (or buffer (current-buffer))))
    ;; Asynchronously construct prompt components
    (apply #'starhugger-query-helper
           config callback
           (starhugger--lambda (&rest
                                async-prompt-result &key context prefix suffix &allow-other-keys)
             (-let*
                 ((code-len (+ (length prefix) (length suffix)))
                  ;; This callback is ran when the request to the language model
                  ;; returns
                  (query-callback
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
                            query-callback
                            :context context
                            :prefix prefix
                            :suffix suffix
                            :num
                            num
                            (append async-prompt-result args)))
                    (:else
                     (funcall query-callback '())))))
               request-obj))
           :prompt nil)))

(cl-defun starhugger--query-until-number (config num callback &rest args &key prompt &allow-other-keys)
  "Using CONFIG, keep querying until NUM of returned choices are achieved.
CALLBACK is called (potentially) multiple time, each time with the
returned choices returned from a request, number of accumulated
choices (including the just returned ones), and the variable rest are
keyword information.  When CALLBACK returns nil, target is reached, or
an error is returned, stop querying.  QUERY-FN: a querying function such
as `starhugger-query' or `starhugger-query-auto-prompt', ARGS: variadic
keyword arguments for QUERY-FN, whose :prompt property will be used on
`starhugger-query''s PROMPT argument.  ARGS are variadic keyword
arguments."
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
                         prompt
                         query-fn-callback
                         :num
                         this-time-num
                         args)))))
      (funcall fetching-loop target-num 0))))

;;; starhugger-core.el ends here

(provide 'starhugger-core)
