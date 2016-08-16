
;;;; init-python.el


(eval-when-compile (require 'python))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-ccr" 'run-python)))
