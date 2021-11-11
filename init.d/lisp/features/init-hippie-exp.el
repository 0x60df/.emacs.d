
;;;; init-hippie-exp.el


(premise init)
(premise bindings)

(overriding-set-key (kbd "C-<tab>") #'hippie-expand)
(overriding-set-key (kbd "ESC C-M-i") #'hippie-expand)


(resolve init-hippie-exp)
