
;;;; init-helm-descbinds.el


(premise init)
(premise init-helm)
(premise inst-helm-descbinds)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "h b") #'helm-descbinds))


(resolve init-helm-descbinds)
