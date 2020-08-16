
;;;; risk.el



;;; base

(premise init)


;;; yes-or-no-p

(defun exit-yes-or-no-p-with-yes-insertion ()
  (interactive)
  (insert "yes")
  (exit-minibuffer))

(defun enable-temporal-key-bind-on-yes-or-no-p (fun &rest args)
  (let* ((key-seq (kbd "C-j"))
         (bound-func  (lookup-key minibuffer-local-map key-seq)))
    (unwind-protect
        (progn
          (define-key minibuffer-local-map key-seq
            #'exit-yes-or-no-p-with-yes-insertion)
          (apply fun args))
      (define-key minibuffer-local-map key-seq
        (if (equal bound-func #'exit-yes-or-no-p-with-yes-insertion)
            'exit-minibuffer
          bound-func)))))

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
