
;;;; init-vterm.el


(premise init)
(premise inst-vterm)

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (if (init-unit-p bindings)
      (add-hook 'vterm-mode-hook (lambda () (balance-mode 0))))

  (define-key vterm-mode-map (kbd "C-@")
    (lambda () (interactive) (vterm-send "C-SPC")))
  (define-key vterm-mode-map (kbd "C-g")
    (lambda () (interactive) (vterm-send "C-g"))))


(resolve init-vterm)
