
;;;; init-dired-x.el


(add-hook 'dired-load-hook
          (lambda () (load "dired-x")))