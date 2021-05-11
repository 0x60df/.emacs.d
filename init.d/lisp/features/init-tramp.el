
;;;; init-tramp.el


(premise init)

(eval-when-compile (require 'tramp-sh))

(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path t))


(resolve init-tramp)
