
;;;; init-scheme.el


(eval-when-compile (require 'scheme))
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map "\C-ccr" 'run-scheme)))
