
;;;; init-flycheck.el


(global-set-key (kbd "C-c !") 'flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (define-key flycheck-mode-map (kbd "C-c ! !") 'flycheck-mode)
     (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
