
;;;; init-helm-swoop.el


(define-key helm-command-map (kbd "C-s") 'helm-swoop)
(define-key isearch-mode-map (kbd "C-i") 'helm-swoop-from-isearch)
