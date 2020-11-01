
;;;; init-helm-projectile.el


(premise init)
(premise custom)
(premise init-helm)
(premise init-projectile)
(premise inst-helm-projectile)

(custom-set-variables
 '(helm-projectile-truncate-lines t))

(define-key helm-command-map (kbd "C-p") #'helm-projectile)


(resolve init-helm-projectile)
