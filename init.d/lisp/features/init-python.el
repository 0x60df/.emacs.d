
;;;; init-python.el


(premise init)

(eval-when-compile (require 'python))
(eval-after-load 'python
  '(progn
     (define-key python-mode-map "\C-ccr" 'run-python)))


(resolve init-python)
