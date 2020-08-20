
;;;; risk.el



;;; base

(premise init)


;;; yes-or-no-p

(define-minor-mode risky-yes-or-no-p-transient-mode
  "Minor mode holding risky keymap for `yes-or-no-p'."
  :global t
  :keymap `(("\C-j" . (lambda ()
                        (interactive)
                        (when (minibufferp (current-buffer))
                          (insert "yes")
                          (exit-minibuffer))))))

(defun with-risky-yes-or-no-p-transient-mode (fun &rest args)
  (unwind-protect
      (progn
        (risky-yes-or-no-p-transient-mode 1)
        (apply fun args))
    (risky-yes-or-no-p-transient-mode -1)))

(define-minor-mode risky-yes-or-no-p-mode
  "Allow answering yes by C-j on `yes-or-no-p'."
  :global t
  :lighter " R"
  (if risky-yes-or-no-p-mode
      (advice-add 'yes-or-no-p :around #'with-risky-yes-or-no-p-transient-mode)
    (advice-remove 'yes-or-no-p #'with-risky-yes-or-no-p-transient-mode)))

(global-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(resolve risk)
