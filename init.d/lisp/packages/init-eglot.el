
;;;; init-eglot.el


(premise init)
(premise inst-eglot)

(eval-when-compile (require 'eglot))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company))


(resolve init-eglot)
