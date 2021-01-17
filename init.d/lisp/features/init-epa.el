
;;;; init-epa.el


(premise init)

(with-eval-after-load 'epa
  (mapc (lambda (hook)
          (add-hook hook (lambda () (setq show-trailing-whitespace nil))))
        '(epa-key-mode-hook epa-info-mode-hook epa-key-list-mode-hook)))


(resolve init-epa)
