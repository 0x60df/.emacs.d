
;;;; init-cmuscheme.el


(premise init)

(eval-after-load 'cmuscheme
  '(add-hook 'inferior-scheme-mode-hook (lambda () (setq mode-name "InfScm"))))


(resolve init-cmuscheme)
