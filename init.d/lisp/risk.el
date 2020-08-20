
;;;; risk.el



;;; base

(premise init)


;;; yes-or-no-p

(defface risky-yes-or-no-p-prefix-face
  '((t :inherit minibuffer-prompt
       :weight bold))
  "Face for prefix of risky-yes-or-no-p prompt.")

(define-minor-mode risky-yes-or-no-p-transient-mode
  "Minor mode holding risky keymap for `yes-or-no-p'."
  :global t
  :keymap `(("\C-j" . (lambda ()
                        (interactive)
                        (when (minibufferp (current-buffer))
                          (insert "yes")
                          (exit-minibuffer))))))

(defun with-risky-yes-or-no-p-transient-mode (fun arg)
  (unwind-protect
      (progn
        (risky-yes-or-no-p-transient-mode 1)
        (funcall fun (concat (propertize "[RISKY] "
                                         'face 'risky-yes-or-no-p-prefix-face)
                             arg)))
    (risky-yes-or-no-p-transient-mode -1)))

(define-minor-mode risky-yes-or-no-p-mode
  "Allow answering yes by C-j on `yes-or-no-p'."
  :global t
  (if risky-yes-or-no-p-mode
      (advice-add 'yes-or-no-p :around #'with-risky-yes-or-no-p-transient-mode)
    (advice-remove 'yes-or-no-p #'with-risky-yes-or-no-p-transient-mode)))

(global-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(resolve risk)
