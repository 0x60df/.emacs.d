
;;;; init-scheme.el


(eval-when-compile (require 'scheme))
(eval-after-load 'scheme
  `(progn
     (define-key scheme-mode-map "\C-ccr" 'run-scheme)))


(resolve init-scheme)
