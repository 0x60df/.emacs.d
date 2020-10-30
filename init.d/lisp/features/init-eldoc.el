
;;;; init-eldoc.el


(premise init)
(premise custom)

(custom-set-variables
 '(eldoc-idle-delay 0.4)
 '(eldoc-minor-mode-string " ElD"))


(resolve init-eldoc)
