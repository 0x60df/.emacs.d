
;;;; init-helm-ls-git.el


(premise init)
(premise bindings)
(premise init-helm)
(premise inst-helm-ls-git)

(define-key helm-command-map (kbd "C-v") #'helm-ls-git-ls)


(resolve init-helm-ls-git)
