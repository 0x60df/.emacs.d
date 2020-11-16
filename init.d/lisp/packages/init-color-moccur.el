
;;;; init-color-moccur.el


(premise init)
(premise custom)
(premise bindings)
(premise feature)
(premise inst-color-moccur)

(eval-when-compile (require 'ibuffer))
(lazy-autoload 'moccur "color-moccur")

(custom-set-variables
 '(moccur-split-word t))

(overriding-set-key (kbd "C-c s m") 'moccur)


(resolve init-color-moccur)
