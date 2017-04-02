
;;;; init-ace-jump-mode.el


(premise init)
(premise inst-ace-jump-mode)

(global-set-key (kbd "M-g M-j") 'ace-jump-mode)
(global-set-key (kbd "H-g") 'ace-jump-mode)


(resolve init-ace-jump-mode)
