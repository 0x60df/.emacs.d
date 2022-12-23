
;;;; init-embark.el


(premise init)
(premise bindings)
(premise inst-embark)

(overriding-set-key (kbd "C-^") #'embark-act)
(overriding-set-key (kbd "C-~") #'embark-dwim)
(overriding-set-key (kbd "C-o b") #'embark-bindings)

(let ((binding (lookup-key help-map (kbd "b"))))
  (if (commandp binding)
      (define-key help-map (kbd "B") binding)))
(define-key help-map (kbd "b") #'embark-bindings)

(dolist (key (list (kbd "C-o b"))) (add-to-list 'balance-mode-key-list key))

(add-to-list 'balance-mode-key-alias-alist `(,(kbd "o SPC b") . ,(kbd "o b")))

(add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (when (or (string-equal (buffer-name) "*Messages*")
                        (eq major-mode #'help-mode)
                        (eq major-mode 'emacs-lisp-compilation-mode)
                        (eq major-mode #'dired-mode)
                        (eq major-mode 'org-agenda-mode)
                        (eq major-mode 'magit-status-mode))
                (let ((entry (lookup-key (current-local-map) (kbd "o"))))
                  (if (and entry (not (numberp entry)))
                      (define-key overriding-balance-weight-mode-map
                        (kbd "oo") entry)))

                (balance-mode-implement-keys
                 (list (kbd "C-o b"))
                 overriding-balance-weight-mode-map)
                (balance-mode-alias-keys
                 `((,(kbd "o SPC b") . ,(kbd "o b")))
               overriding-balance-weight-mode-map))))

(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(resolve init-embark)
