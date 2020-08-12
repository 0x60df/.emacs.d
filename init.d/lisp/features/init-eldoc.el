
;;;; init-eldoc.el


(premise init)

(eval-after-load 'eldoc
  '(custom-set-variables '(eldoc-idle-delay 0.4)))


(resolve init-eldoc)
