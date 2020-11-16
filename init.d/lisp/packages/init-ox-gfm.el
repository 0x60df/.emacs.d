
;;;; init-ox-gfm.el


(premise init)
(premise inst-ox-gfm)

(with-eval-after-load 'ox
  (require 'ox-gfm))


(resolve init-ox-gfm)
