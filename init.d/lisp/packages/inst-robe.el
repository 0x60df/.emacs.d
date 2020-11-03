
;;;; inst-robe.el


(premise init)
(premise init-el-get)

(el-get-bundle robe-mode)
(remove-hook 'ruby-mode-hook #'robe-mode)


(resolve inst-robe)
