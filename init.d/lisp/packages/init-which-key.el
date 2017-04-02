
;;;; init-which-key.el


(premise init)
(premise inst-which-key)

(eval-after-load 'which-key
  '(custom-set-variables '(which-key-idle-delay 0.8)))

(which-key-mode 1)


(resolve init-which-key)
