
;;;; init-ag.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-ag)

(custom-set-variables
 '(ag-group-matches nil))

(overriding-set-key (kbd "C-c s a") #'ag)
(overriding-set-key (kbd "M-s a") #'ag)


(resolve init-ag)
