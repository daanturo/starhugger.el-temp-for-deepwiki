;; -*- lexical-binding: t; -*-

;; Adapter for llm.el

(require 'llm)
(require 'llm-openai)

(require 'starhugger)

(defclass my-llm-el-starhugger-config (starhugger-config-instruct-type-model)
  ((my-provider :initarg :my-provider :initform nil :documentation nil)
   (model :initform "")))

(cl-defmethod starhugger-query
  ((config my-llm-el-starhugger-config)
   prompt
   callback
   &rest
   kwargs
   &key
   buffer
   caller
   &allow-other-keys)
  (apply #'starhugger-query-helper
         config callback
         (cl-function
          (lambda (wrapped-callback
                   cancel-fn-recorder
                   &rest
                   prompt-comps-result-args
                   &key
                   &allow-other-keys)
            (-let* (((&alist 'messages messages)
                     (apply #'starhugger-make-prompt-parameters-default
                            config
                            prompt-comps-result-args))
                    ;; Assuming the list of messages contains some consecutive system
                    ;; prompts, and the last element is the user prompt
                    (joined-system-prompt
                     (string-join (--map (alist-get 'content it) (-butlast messages))
                                  "\n"))
                    (user-prompt
                     (if (stringp prompt)
                         prompt
                       (string-join (--map (alist-get 'content it) (-last-item messages))
                                    "\n"))))
              (letrec ((llm-req
                        (llm-chat-async
                         (starhugger-get config 'my-provider)
                         (llm-make-chat-prompt
                          user-prompt
                          :context joined-system-prompt)
                         (lambda (answer &rest _)
                           (funcall wrapped-callback (list answer)))
                         (lambda (&rest err)
                           (funcall wrapped-callback nil :error err)))))
                (funcall cancel-fn-recorder
                         (lambda () (llm-cancel-request llm-req)))))))
         kwargs))

(setq my-llm-el-starhugger-instance
      (my-llm-el-starhugger-config
       :my-provider
       (make-llm-openai-compatible
        :url "http://localhost:11434/v1/"
        :chat-model "qwen3:0.6b"
        :default-chat-non-standard-params
        '((reasoning_effort . "low")))))
