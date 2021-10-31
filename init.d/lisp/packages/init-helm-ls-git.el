
;;;; init-helm-ls-git.el


(premise init)
(premise advice)
(premise init-helm)
(premise inst-helm-ls-git)

(advice-add-for-once 'helm-browse-project :before
                     (lambda (&rest _args)
                       (require 'helm-ls-git)))
(with-eval-after-load 'helm-global-bindings
 (define-key helm-command-map (kbd "C-v") #'helm-browse-project))


(resolve init-helm-ls-git)
