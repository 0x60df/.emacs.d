
;;;; init-helm-frames.el


(premise init)
(premise init-helm)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd ".") #'helm-frames)

  (with-eval-after-load 'server
    (define-key helm-command-map (kbd ".") #'helm-frames-on-selected-client)
    (define-key helm-command-map (kbd "s-.")
      #'helm-typical-frames-on-each-client)))


(resolve init-helm-frames)
