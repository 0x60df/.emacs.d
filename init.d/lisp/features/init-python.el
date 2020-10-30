
;;;; init-python.el


(premise init)

(eval-when-compile (require 'python))

(with-eval-after-load 'python
  (define-key python-mode-map "\C-ccr" #'run-python))


(resolve init-python)
