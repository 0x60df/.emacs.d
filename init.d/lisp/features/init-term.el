
;;;; init-term.el


(premise init)
(premise whitespace)

(with-eval-after-load 'term
  (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil))))


(resolve init-term)
