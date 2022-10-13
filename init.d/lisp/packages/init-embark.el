
;;;; init-embark.el


(premise init)
(premise bindings)
(premise inst-embark)

(overriding-set-key (kbd "C-^") #'embark-act)
(overriding-set-key (kbd "C-~") #'embark-dwim)
(overriding-set-key (kbd "C-q h b") #'embark-bindings)

(let ((binding (lookup-key help-map (kbd "b"))))
  (if (commandp binding)
      (define-key help-map (kbd "B") binding)))
(define-key help-map (kbd "b") #'embark-bindings)

(dolist (key (list (kbd "C-q h b"))) (add-to-list 'balance-mode-key-list key))

(add-to-list 'balance-mode-key-alias-alist `(,(kbd "q SPC h") . ,(kbd "q h")))

(add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (when (or (string-equal (buffer-name) "*Messages*")
                        (eq major-mode #'help-mode)
                        (eq major-mode 'emacs-lisp-compilation-mode)
                        (eq major-mode #'dired-mode)
                        (eq major-mode 'org-agenda-mode)
                        (eq major-mode 'magit-status-mode))
                (let ((entry (lookup-key (current-local-map) (kbd "q"))))
                  (if (and entry (not (numberp entry)))
                      (define-key overriding-balance-weight-mode-map
                        (kbd "qq") entry)))

                (balance-mode-implement-keys
                 (list (kbd "C-q h b"))
                 overriding-balance-weight-mode-map)
                (balance-mode-alias-keys
                 `((,(kbd "q SPC h b") . ,(kbd "q h b")))
               overriding-balance-weight-mode-map))))

(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(resolve init-embark)
