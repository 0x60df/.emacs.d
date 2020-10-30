
;;;; init-scratch.el


(premise init)
(premise bindings)

(global-set-key (kbd "C-c b") 'scratch)

(with-eval-after-load 'scratch
  (overriding-set-key (kbd "C-l b s") #'scratch-shred-all)
  (define-key scratch-mode-map (kbd "C-c k") #'scratch-shred)
  (define-key scratch-mode-map (kbd "C-l b l") #'scratch-label))


(resolve scratch)
