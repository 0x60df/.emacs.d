
;;;; risk.el



;;; base

(premise init)


;;; yes-or-no-p

(defvar yes-or-no-p-active-p nil
  "whether `yes-or-no-p' is being executed or not.")

(define-key minibuffer-local-map
  (kbd "y") (lambda ()
              (interactive)
              (cond (yes-or-no-p-active-p (insert "yes"))
                    (t (self-insert-command 1)))))
(define-key minibuffer-local-map
  (kbd "n") (lambda ()
              (interactive)
              (cond (yes-or-no-p-active-p (insert "no"))
                    (t (self-insert-command 1)))))

(defun follow-yes-or-no-p-active-p (fun &rest args)
  (setq yes-or-no-p-active-p t)
  (unwind-protect (apply fun args)
    (setq yes-or-no-p-active-p nil)))

(advice-add 'yes-or-no-p :around 'follow-yes-or-no-p-active-p)


(resolve risk)
