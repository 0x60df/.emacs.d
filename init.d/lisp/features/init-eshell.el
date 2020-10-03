
;;;; init-eshell.el


(premise init)

(defun eshell/ediff (file-A file-B) (ediff file-A file-B))

(global-set-key "\C-ct" 'eshell)


(resolve init-eshell)
