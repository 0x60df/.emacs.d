
;;;; init-helm-ag.el


(premise init)
(premise bindings)
(premise init-helm)
(premise init-ag)
(premise inst-helm-ag)


(define-key helm-command-map (kbd "C-a") #'helm-ag)


(resolve init-helm-ag)
