
;;;; init-helm-swoop.el


(premise init)
(premise init-helm)
(premise inst-helm-swoop)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "C-s") #'helm-swoop))
(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-q") (make-sparse-keymap))
  (define-key isearch-mode-map (kbd "C-q C-q") #'isearch-quote-char)
  (define-key isearch-mode-map (kbd "C-q C-s") #'helm-swoop-from-isearch)
  (define-key isearch-mode-map (kbd "H-q") #'helm-swoop-from-isearch))


(resolve init-helm-swoop)
