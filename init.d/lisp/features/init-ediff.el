
;;;; init-ediff.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-default))

(overriding-set-key (kbd "C-c d f") #'ediff-files)
(overriding-set-key (kbd "C-c d b") #'ediff-buffers)
(overriding-set-key (kbd "C-c d r") #'ediff-regions-linewise)


(resolve init-ediff)
