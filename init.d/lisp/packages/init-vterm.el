
;;;; init-vterm.el


(premise init)
(premise inst-vterm)

(defvar vterm-mode-map)

(declare-function vterm-send "vterm")

(define-minor-mode overriding-vterm-ctl-o-mode
  "Minor mode to enable overriding keymap for C-o."
  :keymap `((,(kbd "C-o") . ,(lambda () (interactive) (vterm-send "C-o")))))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (if (init-unit-p bindings)
      (add-hook 'vterm-mode-hook (lambda () (balance-mode 0))))

  (push `(overriding-vterm-ctl-o-mode . ,overriding-vterm-ctl-o-mode-map)
        overriding-reserved-key-map-alist)
  (add-hook 'vterm-mode-hook #'overriding-vterm-ctl-o-mode)

  (define-key vterm-mode-map (kbd "C-g")
    (lambda () (interactive) (vterm-send "C-g"))))


(resolve init-vterm)
