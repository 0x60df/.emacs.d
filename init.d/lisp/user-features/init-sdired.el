
;;;; init-sdired.el


(premise init)

(with-eval-after-load 'dired
  (define-key dired-mode-map "s" 'sdired-sort))


(resolve sdired)
