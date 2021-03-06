
;;;; init-eshell.el


(premise init)
(premise bindings)

(defun eshell/ediff (file-A file-B)
  "ediff for eshell command."
  (ediff file-A file-B))

(overriding-set-key (kbd "C-c t") #'eshell)

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil))))


(resolve init-eshell)
