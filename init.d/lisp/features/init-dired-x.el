
;;;; init-dired-x.el


(premise init)

(with-eval-after-load 'dired
  (load "dired-x" nil t))


(resolve init-dired-x)
