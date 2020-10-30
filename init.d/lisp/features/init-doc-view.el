
;;;; init-doc-view.el


(premise init)

(add-hook 'doc-view-mode-hook #'auto-revert-mode)


(resolve init-doc-view)
