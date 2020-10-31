
;;;; init-smex.el


(premise init)
(premise bindings)
(premise inst-smex)


(overriding-set-key (kbd "M-x") #'smex)
(overriding-set-key (kbd "M-X") #'execute-extended-command)


(resolve init-smex)
