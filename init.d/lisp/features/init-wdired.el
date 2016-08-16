
;;;; init-wdired.el


(eval-when-compile (require 'dired))
(eval-after-load 'dired
  `(progn
     (require 'wdired)
     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

