
;;;; init-helm-ag.el


(premise init)
(premise init-helm)
(premise init-ag)
(premise inst-helm-ag)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "C-a") #'helm-ag))


(resolve init-helm-ag)
