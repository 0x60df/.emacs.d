
;;;; init-term.el


(premise init)

(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))


(resolve init-term)
