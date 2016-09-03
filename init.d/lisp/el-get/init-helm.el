
;;;; init-helm.el



;;; base

(add-hook 'helm-mode-hook (lambda () (icomplete-mode -1)))
(setq helm-completion-mode-string " H")


;;; bindings

(custom-set-variables '(helm-command-prefix-key "C-q"))
(define-key helm-command-map (kbd "C-q") 'quoted-insert)
(define-key helm-command-map (kbd "C-m") 'helm-mini)
(define-key helm-command-map (kbd "C-f") 'helm-find-files)
(define-key helm-command-map (kbd "C-b") 'helm-buffers-list)
(define-key helm-command-map (kbd "M-x") 'helm-M-x)
(define-key helm-command-map (kbd "C-o") 'helm-occur)
