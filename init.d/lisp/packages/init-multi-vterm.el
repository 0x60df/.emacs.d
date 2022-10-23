
;;;; init-multi-vterm.el


(premise init)
(premise inst-multi-vterm)
(premise bindings)

(overriding-set-key (kbd "H-t") #'multi-vterm)
(overriding-set-key (kbd "C-l t") #'multi-vterm)

(add-to-list 'balance-mode-key-list (kbd "C-l t"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC t") . ,(kbd "l t")))


(resolve init-multi-vterm)
