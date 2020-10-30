
;;;; init-scheme.el


(premise init)

(eval-when-compile (require 'scheme))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map "\C-ccr" #'run-scheme))


(resolve init-scheme)
