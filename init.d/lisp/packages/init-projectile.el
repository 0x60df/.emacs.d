
;;;; init-projectile.el


(premise init)
(premise inst-projectile)

(eval-when-compile (require 'projectile))

(eval-after-load 'projectile
  '(progn
     (custom-set-variables
      '(projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " P"
                  (format " P[%s]" (projectile-project-name))))))

     (define-key projectile-command-map (kbd "p") 'projectile-mode)
     (define-key projectile-command-map (kbd "SPC")
       'projectile-switch-project)))

(global-set-key (kbd "C-c p") 'projectile-mode)


(resolve init-projectile)
