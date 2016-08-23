
;;;; init-smex.el



;;; base

(smex-initialize)


;;; bindings

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-s-x") 'execute-extended-command)
