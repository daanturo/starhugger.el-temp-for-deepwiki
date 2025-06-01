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

(require 'starhugger-core)

;;;; Customization

(defgroup starhugger nil
  "LLM-powered code completion client."
  :group 'external)


(defcustom starhugger-enable-spinner t
  "Show spinner when fetching interactively."
  :type 'boolean)

(defcustom starhugger-suggestion-list-size 32
  "Maximum number of saved suggestions in current buffer.
Note that this includes all recently fetched suggestions so not
all of them are relevant all the time."
  :type 'natnum)

(defcustom starhugger-dismiss-suggestion-after-change t
  "Whether to clear the overlay when text changes and not partially accepted."
  :type 'boolean)

(defcustom starhugger-auto-idle-time 1.0
  "Seconds to wait after typing, before fetching.
For `starhugger-auto-mode'."
  :type 'float)

(defcustom starhugger-auto-dismiss-when-move-out t
  "Whether to dismiss suggestion when moving point outside."
  :type 'boolean)

(defvar starhugger-model-id nil
  "Obsolete, retain for old configurations.")
(make-obsolete-variable 'starhugger-model-id nil "0.7.0")

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


;;;; Overlay inline suggestion

(declare-function spinner-start "spinner")
(defvar spinner-types)
(defvar starhugger--spinner-added-type nil)
(defun starhugger--spinner-start ()
  (unless starhugger--spinner-added-type
    (require 'spinner)
    (push '(starhugger . ["⭐" "🌟"]) spinner-types)
    (setq starhugger--spinner-added-type t))
  (spinner-start 'starhugger 3))

(defvar-local starhugger--overlay nil)

;; We may as well use a ring (`make-ring'), but it doesn't have a built-in way
;; to modify elements in-place
(defvar-local starhugger--suggestion-list '()
  "List of recently fetched suggestions along with internal state.
Recent suggestions are added to the beginning.")

(defvar-local starhugger--inline-inhibit-changing-overlay nil)

(defun starhugger-inlining--after-change-h (&optional _beg _end _old-len)
  (when (and this-command
             (not starhugger--inline-inhibit-changing-overlay)
             starhugger-dismiss-suggestion-after-change)
    (starhugger-dismiss-suggestion nil)))

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

(defun starhugger--current-overlay-suggestion ()
  (overlay-get starhugger--overlay 'starhugger-ovlp-current-suggestion))

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

(defun starhugger--suggestion-positional-state (&optional pt)
  (if pt
      (save-excursion
        (goto-char pt)
        (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun starhugger--relevant-fetched-suggestions (&optional all pt)
  (-let* ((cur-state (starhugger--suggestion-positional-state pt)))
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
          (cur-state (starhugger--suggestion-positional-state))
          (recent-suggt
           (-some
            (-lambda ((state suggt))
              (and (equal cur-state state) suggt))
            starhugger--suggestion-list)))
    (when recent-suggt
      (starhugger--ensure-inlininng-mode)
      (when starhugger-debug
        (message "%s at %s:%d:%d : %s"
                 #'starhugger--try-show-most-recent-suggestion
                 (buffer-name)
                 (line-number-at-pos)
                 (current-column)
                 recent-suggt))
      (starhugger--init-overlay recent-suggt pt))
    recent-suggt))

(defvar starhugger--running-request-table--table nil
  "Keys are buffers.")
(defun starhugger--running-request-table ()
  (unless starhugger--running-request-table--table
    (setq starhugger--running-request-table--table
          (make-hash-table :test #'equal)))
  starhugger--running-request-table--table)

(defvar starhugger-interactive-config-instance
  (starhugger-config-openai-compat-base-completions
   :model
   (or starhugger-model-id "qwen2.5-coder:14b-base")
   :join-prompts " "
   :num 3))
(defvar starhugger-auto-config-instance
  (starhugger-config-openai-compat-base-completions
   :model
   (or starhugger-model-id "qwen2.5-coder:3b-base")
   :join-prompts " "))

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key interact callback config &allow-other-keys)
  "Show code suggestions as overlay(s).
CONFIG: a `starhugger-config' instance, interactively defaults to
`starhugger-interactive-config-instance', else
`starhugger-auto-config-instance'.  This command tries to request
CONFIG's :num completion choices in total.  When an inline suggestion is
already displayed and there are more fetched choices,
`starhugger-show-next-suggestion' can be used switch to switch to
others.  As some providers only support getting 1 choice, more fetching
will be done in the background until the targeted amount is received.
Non-nil INTERACT: show spinner.  CALLBACK: function whose argument list
is (content-choices &rest kwargs), it may be called multiple times due
to multiple fetches."
  (interactive (list
                :interact t
                :force-new starhugger-inlining-mode
                :config starhugger-interactive-config-instance))
  (-let* ((config (or config starhugger-auto-config-instance))
          (orig-buf (current-buffer))
          (orig-pt (point))
          (state (starhugger--suggestion-positional-state))
          (target-num (slot-value config 'num))
          (spin-stop-maybe
           (or (and interact
                    starhugger-enable-spinner
                    (starhugger--spinner-start))
               #'ignore))
          (request-record nil))
    (starhugger--query-until-number
     config target-num
     (starhugger--lambda (content-choices
                          accumulated-num
                          &rest
                          query-until-number-result
                          &key
                          error
                          &allow-other-keys)
       (when callback
         (apply callback content-choices query-until-number-result))
       (cond
        ((not (buffer-live-p orig-buf)))
        (:else
         (with-current-buffer orig-buf
           (-when-let* ((_ (< 0 (length content-choices)))
                        (1st-choice (seq-first content-choices))
                        ;; Only display and continue when didn't move or
                        ;; interactive, in that case we are explicitly waiting
                        (_ (or interact (= orig-pt (point)))))
             (starhugger--ensure-inlininng-mode)
             (starhugger--add-to-suggestion-list content-choices state)
             ;; Initial request
             (when (= accumulated-num (length content-choices))
               (starhugger--init-overlay 1st-choice orig-pt)))
           (when (or error (<= target-num accumulated-num))
             (cl-callf
                 (lambda (lst) (delete request-record lst))
                 (gethash orig-buf (starhugger--running-request-table) '()))
             (funcall spin-stop-maybe))
           (when (not (starhugger--active-overlay-p))
             (starhugger--ensure-inlininng-mode 0))))))
     :caller #'starhugger-trigger-suggestion)))

;; NOTE TODO PROBLEM: Trying record the request, to cancel them when needed.
;; But the chain looks like this:

;; Trigger -> Async context prompt starts -> Async context prompt ends -> Async
;; LM request starts (*) -> Async LM request ends, content choices

;; It's impossible(?) to get the LM request object when it starts.

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
                (dlet ((inhibit-message t))
                  (message "`%s' Already running %d requests in %S!"
                           'starhugger--triggger-suggestion-prefer-cache
                           (length requests)
                           (buffer-name))))))
          (starhugger-trigger-suggestion
           :callback callback
           :config starhugger-auto-config-instance)))))

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

(cl-defun starhugger-dismiss-suggestion (&optional (stop-fetching t))
  "Clear current suggestion.
Non-nil STOP-FETCHING (true by default): also cancel unfinished
requests."
  (interactive (list (not current-prefix-arg)))
  (when stop-fetching
    (starhugger--cancel-requests-in-buffer (current-buffer)))
  (starhugger-inlining-mode 0))

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
      (-let* ((suggestion (starhugger--current-overlay-suggestion))
              (text-to-insert
               (with-temp-buffer
                 (insert suggestion)
                 (goto-char (point-min))
                 (apply by args)
                 (buffer-substring (point-min) (point)))))
        (insert text-to-insert)
        ;; If the whole suggestion is consumed, clear the overlay, else remove
        ;; the accepted part from the overlay's current start
        (if (equal suggestion text-to-insert)
            (progn
              (starhugger-dismiss-suggestion nil))
          (progn
            (starhugger--update-overlay
             (string-remove-prefix text-to-insert suggestion))))
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
      ;; Point isn't in the overlay range anymore: clear it
      (when (not (and (numberp beg) (numberp end) (<= beg (point) end)))
        (starhugger-dismiss-suggestion nil)))))

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

;;; starhugger.el ends here

(provide 'starhugger)

;; Local Variables:
;; byte-compile-docstring-max-column: 200
;; End:
