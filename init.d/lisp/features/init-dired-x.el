
;;;; init-dired-x.el


(premise init)

(add-hook 'dired-load-hook (lambda () (load "dired-x" nil t)))


(resolve init-dired-x)
