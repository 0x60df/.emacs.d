
;;;; init-eshell.el


(premise init)

(defun eshell/ediff (file-A file-B) (ediff file-A file-B))

(global-set-key "\C-ct" 'eshell)

(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))


(resolve init-eshell)
