
;;;; init-helm-ls-git.el


(premise init)
(premise init-helm)
(premise inst-helm-ls-git)

(with-eval-after-load 'helm-global-bindings
 (define-key helm-command-map (kbd "C-v") #'helm-ls-git-ls))


(resolve init-helm-ls-git)
