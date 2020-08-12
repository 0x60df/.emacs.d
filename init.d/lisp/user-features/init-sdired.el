
;;;; init-sdired.el


(premise init)

(eval-after-load 'dired
  '(define-key dired-mode-map "s" 'sdired-sort))


(resolve sdired)
