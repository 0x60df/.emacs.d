
;;;; init-helm-descbinds.el


(premise init)
(premise bindings)
(premise init-helm)
(premise inst-helm-descbinds)

(with-eval-after-load 'helm-global-bindings
  (define-key helm-command-map (kbd "h b") #'helm-descbinds))
(add-to-list 'balance-mode-key-list (kbd "C-q h b"))
(add-to-list 'balance-mode-key-alias-alist
             `(,(kbd "q SPC h b") . ,(kbd "q h b")))

(add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (when (or (string-equal (buffer-name) "*Messages*")
                        (eq major-mode #'help-mode)
                        (eq major-mode 'emacs-lisp-compilation-mode)
                        (eq major-mode #'dired-mode)
                        (eq major-mode 'org-agenda-mode)
                        (eq major-mode 'magit-status-mode))
                (balance-mode-implement-keys
                 (list (kbd "C-q h b"))
                 overriding-balance-weight-mode-map)
                (balance-mode-alias-keys
                 `((,(kbd "q SPC h b") . ,(kbd "q h b")))
                 overriding-balance-weight-mode-map))))


(resolve init-helm-descbinds)
