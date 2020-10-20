
;;;; init-ediff.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-default))

(global-set-key (kbd "C-c d f") #'ediff-files)
(global-set-key (kbd "C-c d b") #'ediff-buffers)
(global-set-key (kbd "C-c d r") #'ediff-regions-linewise)


(resolve init-ediff)
