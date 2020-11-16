
;;;; init-doc-view.el


(premise init)

(with-eval-after-load 'doc-view
  (add-hook 'doc-view-mode-hook #'auto-revert-mode))


(resolve init-doc-view)
