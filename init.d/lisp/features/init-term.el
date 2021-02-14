
;;;; init-term.el


(premise init)

(with-eval-after-load 'term
  (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (advice-add 'term-char-mode :after (lambda (&rest args)
                                       (setq-local global-hl-line-mode nil)
                                       (global-hl-line-unhighlight)))
  (advice-add 'term-line-mode :after (lambda (&rest args)
                                       (setq-local global-hl-line-mode t))))


(resolve init-term)
