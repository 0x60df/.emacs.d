
;;;; init-elisp-mode.el


(premise init)

(with-eval-after-load 'elisp-mode
  (add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp I")))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E-Lisp"))))


(resolve init-elisp-mode)
