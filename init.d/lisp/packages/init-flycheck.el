
;;;; init-flycheck.el


(premise init)
(premise inst-flycheck)

(eval-when-compile (require 'flycheck))

(global-set-key (kbd "C-c !") 'flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (define-key flycheck-mode-map (kbd "C-c ! !") 'flycheck-mode)
     (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))


(resolve init-flycheck)
