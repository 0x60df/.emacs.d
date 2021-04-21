
;;;; init-smex.el


(premise init)
(premise bindings)
(premise inst-smex)


(overriding-set-key (kbd "M-x") #'smex)
(overriding-set-key (kbd "M-X") #'execute-extended-command)

(add-hook 'emacs-startup-hook #'smex-initialize)


(resolve init-smex)
