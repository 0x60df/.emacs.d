
;;;; init-python.el


(premise init)

(eval-when-compile (require 'python))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c c r") #'run-python))


(resolve init-python)
