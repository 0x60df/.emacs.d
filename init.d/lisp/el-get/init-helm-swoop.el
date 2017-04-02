
;;;; init-helm-swoop.el


(premise init)
(premise inst-helm-swoop)

(define-key helm-command-map (kbd "C-s") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-i") 'helm-swoop-from-isearch)


(resolve init-helm-swoop)
