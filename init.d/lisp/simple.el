
;;;; simple.el


(premise init)
(premise subr)

(defun duplicate-and-comment ()
  "Duplicate region and comment out them."
  (interactive)
  (if (region-active-p)
      (let ((p (point))
            (rb (region-beginning))
            (re (region-end)))
        (goto-char rb)
        (kill-ring-save rb re)
        (comment-region rb re)
        (yank)
        (newline-and-indent)
        (goto-char p))))

(defun comment-switch ()
  "Scan region, then uncomment whole comment lines and comment others."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (goto-char end)
          (catch 'done
            (while t
              (save-excursion
                (goto-char beg)
                (comment-forward (- end beg))
                (if (< beg (point))
                    (if (< (point) end)
                        (uncomment-region beg (point))
                      (uncomment-region beg end)
                      (throw 'done t))
                  (let ((med (comment-search-forward end 'noerror))
                        (test (lambda (point)
                                (save-excursion
                                  (beginning-of-line)
                                  (comment-forward)
                                  (< (point) point)))))
                    (while (and med (funcall test med))
                      (setq med (comment-search-forward end 'noerror)))
                    (if med
                        (progn
                          (goto-char med)
                          (comment-region beg med))
                      (comment-region beg end)
                      (throw 'done t))))
                (setq beg (point)))
              (setq end (point))))))))

(defun yank-pop-reverse (&optional n)
  "Yank pop by reverse direction.
The N-th next kill is inserted.
If called with no argument, insert the next kill"
  (interactive "p")
  (unless n (setq n 1))
  (yank-pop (- n)))

(defun next-line-scroll-up (&optional n)
  "Move cursor to next screen line and scroll up.
Increment is N. If N is ommited, use 1."
  (interactive "p")
  (unless n (setq n 1))
  (if (eql (line-number-at-pos) (line-number-at-pos
                                 (let ((we (1- (window-end))))
                                   (if (zerop we)
                                       1
                                     we))))
      (progn
        (scroll-up n)
        (vertical-motion n))
    (vertical-motion n)
    (scroll-up n)))

(defun previous-line-scroll-down (&optional n)
  "Move cursor to previous screen line and scroll down.
Increment is N. If N is ommited, use 1."
  (interactive "p")
  (unless n (setq n 1))
  (if (eql (line-number-at-pos) (line-number-at-pos (window-start)))
      (progn
        (scroll-down n)
        (vertical-motion (- n)))
    (vertical-motion (- n))
    (scroll-down n)))

(defvar search-char-in-line-matched nil
  "Matched char by search-char-in-line functions.
`search-forward-char-in-line' and
`search-backward-char-in-line' use this.")

(defun search-forward-char-in-line (char)
  "Search forward character on a current line from cursor position.
CHAR is a character in search.
If called interactively, this function `read-char' for CHAR.
If CHAR is found successfully, consecutive call of this
commandor wrapper command who calls this command searches
the same character."
  (interactive
   (list (cond ((and (eq this-command last-command)
                     search-char-in-line-matched)
                search-char-in-line-matched)
               (t (read-char "Char: ")))))
  (let (success)
    (unwind-protect
        (when (and (characterp char)
                   (search-forward
                    (char-to-string char) (line-end-position) t
                    (if (char-equal char (following-char)) 2 1)))
          (backward-char)
          (setq success t))
      (setq search-char-in-line-matched (if success char nil)))))

(defun search-backward-char-in-line (char)
  "Search backward character on a current line from cursor position.
CHAR is a character in search.
If called interactively, this function `read-char' for CHAR.
If CHAR is found successfully, consecutive call of this
commandor wrapper command who calls this command searches
the same character."
  (interactive
   (list (cond ((and (eq this-command last-command)
                     search-char-in-line-matched)
                search-char-in-line-matched)
               (t (read-char "Char: ")))))
  (let (success)
    (unwind-protect
        (when (and (characterp char)
                   (search-backward
                    (char-to-string char) (line-beginning-position) t))
          (setq success t))
      (setq search-char-in-line-matched (if success char nil)))))

(defun search-forward-char-in-line-consecutively ()
  "`search-forward-char-in-line' consecutively."
  (interactive)
  (call-interactively #'search-forward-char-in-line)
  (unless consecutive-search-forward-char-in-line-mode
    (consecutive-search-forward-char-in-line-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-search-forward-char-in-line-mode 0))))

(define-minor-mode consecutive-search-forward-char-in-line-mode
  "Minor mode for consecutive character forward search."
  :group 'user
  :global t
  :keymap '(("f" . search-forward-char-in-line-consecutively)))

(defun search-backward-char-in-line-consecutively ()
  "`search-backward-char-in-line' consecutively."
  (interactive)
  (call-interactively #'search-backward-char-in-line)
  (unless consecutive-search-backward-char-in-line-mode
    (consecutive-search-backward-char-in-line-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-search-backward-char-in-line-mode 0))))

(define-minor-mode consecutive-search-backward-char-in-line-mode
  "Minor mode for consecutive character backward search."
  :group 'user
  :global t
  :keymap '(("b" . search-backward-char-in-line-consecutively)))

(defun emulate-forward-word ()
  "Emulate `forward-word' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "M-f"))))
    (if (commandp func)
        (call-interactively func))))

(defun emulate-backward-word ()
  "Emulate `backward-word' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "M-b"))))
    (if (commandp func)
        (call-interactively func))))

(defun emulate-forward-sexp ()
  "Emulate `forward-sexp' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "C-M-f"))))
    (if (commandp func)
        (call-interactively func))))

(defun emulate-backward-sexp ()
  "Emulate `backward-sexp' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "C-M-b"))))
    (if (commandp func)
        (call-interactively func))))

(defun emulate-scroll-up-command ()
  "Emulate `scroll-up-command' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "C-v"))))
    (if (commandp func)
        (call-interactively func))))

(defun emulate-scroll-down-command ()
  "Emulate `scroll-down-command' by `call-interactively' keys."
  (interactive)
  (let ((func (key-binding (kbd "M-v"))))
    (if (commandp func)
        (call-interactively func))))

(defvar consecutive-emulate-forward-backward-word-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'emulate-forward-word-consecutively)
    (define-key map (kbd "b") #'emulate-backward-word-consecutively)
    map))

(define-minor-mode consecutive-emulate-forward-backward-word-mode
  "Minor mode for consecutive cursor movement by word."
  :group 'user
  :global t
  :keymap consecutive-emulate-forward-backward-word-mode-map)

(defun emulate-forward-word-consecutively ()
  "`emulate-forward-word' consecutively."
  (interactive)
  (call-interactively #'emulate-forward-word)
  (unless consecutive-emulate-forward-backward-word-mode
    (consecutive-emulate-forward-backward-word-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-emulate-forward-backward-word-mode 0))))

(defun emulate-backward-word-consecutively ()
  "`emulate-backward-word' consecutively."
  (interactive)
  (call-interactively #'emulate-backward-word)
  (unless consecutive-emulate-forward-backward-word-mode
    (consecutive-emulate-forward-backward-word-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-emulate-forward-backward-word-mode 0))))

(defvar consecutive-emulate-forward-backward-sexp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'emulate-forward-sexp-consecutively)
    (define-key map (kbd "b") #'emulate-backward-sexp-consecutively)
    map))

(define-minor-mode consecutive-emulate-forward-backward-sexp-mode
  "Minor mode for consecutive cursor movement by sexp."
  :group 'user
  :global t
  :keymap consecutive-emulate-forward-backward-sexp-mode-map)

(defun emulate-forward-sexp-consecutively ()
  "`emulate-forward-sexp' consecutively."
  (interactive)
  (call-interactively #'emulate-forward-sexp)
  (unless consecutive-emulate-forward-backward-sexp-mode
    (consecutive-emulate-forward-backward-sexp-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-emulate-forward-backward-sexp-mode 0))))

(defun emulate-backward-sexp-consecutively ()
  "`emulate-backward-sexp' consecutively."
  (interactive)
  (call-interactively #'emulate-backward-sexp)
  (unless consecutive-emulate-forward-backward-sexp-mode
    (consecutive-emulate-forward-backward-sexp-mode 1))
  (add-hook-for-once
   'pre-command-hook
   (lambda () (consecutive-emulate-forward-backward-sexp-mode 0))))

(defvar-local auto-overwrite-time 0.5 "Time for performing auto overwrite.")

(defvar-local auto-overwrite-timer nil "Idle timer for auto overwrite.")

(defun auto-overwrite-set-time (seconds)
  "Set `auto-overwrite-time'."
  (interactive
   `(,(read-number (format "%.1f seconds to: " auto-overwrite-time))))
  (setq auto-overwrite-time seconds))

(defun auto-overwrite--start-timer (&rest _)
  "Start timer for auto overwrite."
  (if (timerp auto-overwrite-timer) (cancel-timer auto-overwrite-timer))
  (setq auto-overwrite-timer
        (run-with-timer
         auto-overwrite-time
         nil
         (lambda (buffer)
           (with-current-buffer buffer
             (if (and buffer-file-name (not buffer-read-only))
                 (let ((message (current-message))
                       (postscript (format "Overwrote %s" buffer-file-name)))
                   (let ((inhibit-message t)) (save-buffer))
                   (let ((message-truncate-lines
                          (<= (length message) (frame-width (selected-frame)))))
                     (message
                      (if message
                          (concat message
                                  (propertize (concat " --- " postscript)
                                              'face 'warning))
                        postscript)))))))
         (current-buffer))))

(defun auto-overwrite--postpone-timer ()
  "Postpone timer for auto overwrite."
  (if (timerp auto-overwrite-timer)
      (timer-set-time
       auto-overwrite-timer
       (timer-relative-time (current-time) auto-overwrite-time))))

(define-minor-mode auto-overwrite-mode
  "Minor mode for overwrite buffer automatically."
  :lighter (:eval (let ((lighter " AOvw"))
                    (if (and buffer-file-name (not buffer-read-only))
                        (propertize lighter 'face 'mode-line-warning)
                      lighter)))
  :group 'user
  (if auto-overwrite-mode
      (progn
        (add-hook 'after-change-functions #'auto-overwrite--start-timer nil t)
        (add-hook 'pre-command-hook #'auto-overwrite--postpone-timer nil t))
    (if (timerp auto-overwrite-timer) (cancel-timer auto-overwrite-timer))
    (remove-hook 'after-change-functions #'auto-overwrite--start-timer t)
    (remove-hook 'pre-command-hook #'auto-overwrite--postpone-timer t)))

(defun replace-char ()
  "Read char and replace the caracter under cursor."
  (interactive)
  (let ((new (read-char "New char: ")))
    (save-excursion
      (delete-char 1)
      (insert-char new))))


(resolve simple)
