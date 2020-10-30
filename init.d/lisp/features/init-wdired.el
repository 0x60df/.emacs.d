
;;;; init-wdired.el


(premise init)

(eval-when-compile (require 'dired))

(with-eval-after-load 'dired
  (define-key dired-mode-map "r" #'wdired-change-to-wdired-mode))


(resolve init-wdired)
