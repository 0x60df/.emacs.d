
;;;; init-elisp-mode.el


(premise init)

(eval-after-load 'elisp-mode
  '(progn
    (add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp I")))
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E-Lisp")))))


(resolve init-elisp-mode)
