
;;;; risk.el


(premise init)


;;; yes-or-no-p

(defface risky-yes-or-no-p-prefix-face
  '((t :inherit minibuffer-prompt
       :weight bold))
  "Face for prefix of risky-yes-or-no-p prompt."
  :group 'user)

(defun risky-yes-or-no-p-yes ()
  "Answer yes in minibuffer."
  (interactive)
  (when (minibufferp (current-buffer))
    (insert "yes")
    (exit-minibuffer)))

(define-minor-mode risky-yes-or-no-p-transient-mode
  "Minor mode which is transiently turned on during `yes-or-no-p'."
  :global t
  :keymap `(("\C-j" . ,#'risky-yes-or-no-p-yes)))

(defun with-risky-yes-or-no-p-transient-mode (fun arg)
  (unwind-protect
      (progn
        (risky-yes-or-no-p-transient-mode 1)
        (funcall fun (concat (propertize "[RISKY] "
                                         'face 'risky-yes-or-no-p-prefix-face)
                             arg)))
    (risky-yes-or-no-p-transient-mode -1)))

(define-minor-mode risky-yes-or-no-p-mode
  "Enable answering yes by one key sequence.
\\<risky-yes-or-no-p-transient-mode-map>
\\[risky-yes-or-no-p-yes] which is key binding for `risky-yes-or-no-p-yes'
realise it as `insert' yes and `exit-minibuffer'."
  :global t
  (if risky-yes-or-no-p-mode
      (advice-add 'yes-or-no-p :around #'with-risky-yes-or-no-p-transient-mode)
    (advice-remove 'yes-or-no-p #'with-risky-yes-or-no-p-transient-mode)))

(global-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)
(risky-yes-or-no-p-mode 1)


(resolve risk)
