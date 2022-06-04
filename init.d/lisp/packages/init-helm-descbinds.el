
;;;; init-helm-descbinds.el


(premise init)
(premise bindings)
(premise init-helm)
(premise inst-helm-descbinds)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "h b") #'helm-descbinds))
(add-to-list 'balance-mode-key-list (kbd "C-q h b"))
(add-to-list 'balance-mode-key-alias-alist
             `(,(kbd "q SPC h b") . ,(kbd "q h b")))


(resolve init-helm-descbinds)
