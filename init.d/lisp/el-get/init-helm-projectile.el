
;;;; init-helm-projectile.el


(premise init)
(premise inst-helm-projectile)

(define-key helm-command-map (kbd "C-p") 'helm-projectile)


(resolve init-helm-projectile)
