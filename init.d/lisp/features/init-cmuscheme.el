
;;;; init-cmuscheme.el


(premise init)

(with-eval-after-load 'cmuscheme
  (add-hook 'inferior-scheme-mode-hook (lambda () (setq mode-name "InfScm"))))


(resolve init-cmuscheme)
