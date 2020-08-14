
;;;; risk.el



;;; base

(premise init)


;;; yes-or-no-p

(defun enable-temporal-key-bind-on-yes-or-no-p (fun &rest args)
  (unwind-protect
      (progn
        (define-key minibuffer-local-map
          (kbd "M-RET") (lambda ()
                               (interactive)
                               (insert "yes")
                               (exit-minibuffer)))
        (apply fun args))
    (define-key minibuffer-local-map (kbd "M-RET") nil)))

(define-minor-mode risky-yes-or-no-p-mode
  "Allow yes return by C-return, and no return by M-return."
  :global t
  :lighter " R"
  (if risky-yes-or-no-p-mode
      (advice-add 'yes-or-no-p :around
                  #'enable-temporal-key-bind-on-yes-or-no-p)
    (advice-remove 'yes-or-no-p #'enable-temporal-key-bind-on-yes-or-no-p)))

(global-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(resolve risk)
