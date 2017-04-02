
;;;; init-helm-descbinds.el


(premise init)
(premise inst-helm-descbinds)

(define-key helm-command-map (kbd "h b") 'helm-descbinds)


(resolve init-helm-descbinds)
