
;;;; init-scheme.el


(premise init)

(eval-when-compile (require 'scheme))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c c r") #'run-scheme))


(resolve init-scheme)
