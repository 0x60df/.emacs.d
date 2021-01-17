
;;;; init-info.el


(premise init)

(with-eval-after-load 'info
  (add-hook 'Info-mode-hook (lambda () (setq show-trailing-whitespace nil))))


(resolve init-info)
