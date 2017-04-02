
;;;; init-helm-ag.el


(premise init)
(premise inst-helm-ag)

(define-key helm-command-map (kbd "C-a") 'helm-ag)


(resolve init-helm-ag)
