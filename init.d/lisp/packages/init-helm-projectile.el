
;;;; init-helm-projectile.el


(premise init)
(premise custom)
(premise init-helm)
(premise init-projectile)
(premise inst-helm-projectile)

(custom-set-variables
 '(helm-projectile-truncate-lines t))

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "C-p") #'helm-projectile))


(resolve init-helm-projectile)
