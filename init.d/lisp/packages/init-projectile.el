
;;;; init-projectile.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-projectile)

(eval-when-compile (require 'projectile))

(custom-set-variables
 '(projectile-mode-line-prefix " P"))

(with-eval-after-load 'projectile
  (setq-default projectile--mode-line projectile-mode-line-prefix)
  (define-key projectile-command-map (kbd "p") #'projectile-mode)
  (define-key projectile-command-map (kbd "SPC")
    #'projectile-switch-project))

(overriding-set-key (kbd "C-c v P") #'projectile-mode)


(resolve init-projectile)
