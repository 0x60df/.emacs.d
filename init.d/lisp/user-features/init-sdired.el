
;;;; init-sdired.el


(premise init)
(premise user-feature)

(eval-when-compile (require 'dired))

(with-eval-after-load 'dired
  (define-key dired-mode-map "s" #'sdired-sort))


(resolve sdired)
