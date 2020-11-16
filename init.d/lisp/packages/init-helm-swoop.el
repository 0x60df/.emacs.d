
;;;; init-helm-swoop.el


(premise init)
(premise init-helm)
(premise inst-helm-swoop)

(define-key helm-command-map (kbd "C-s") #'helm-swoop)
(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-i") #'helm-swoop-from-isearch))


(resolve init-helm-swoop)
