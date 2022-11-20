;;; -*- lexical-binding: t -*-
;;;; init-multi-vterm.el


(premise init)
(premise inst-multi-vterm)
(premise bindings)
(premise keyboard)

(overriding-set-key (kbd "H-t") #'multi-vterm)
(overriding-set-key (kbd "C-l t") #'multi-vterm)

(add-to-list 'balance-mode-key-list (kbd "C-l t"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC t") . ,(kbd "l t")))
(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (if (or (memq major-mode '(help-mode
                                       dired-mode
                                       emacs-lisp-compilation-mode))
                    (string-equal (buffer-name) "*Messages*"))
                (let* ((key (kbd "t"))
                       (binding (lookup-key overriding-balance-mode-map key)))
                  (unless (numberp binding)
                    (define-key overriding-balance-mode-map key
                      (lambda ()
                        (interactive)
                        (if balance-mode-transient-hyper
                            (progn
                              (balance-mode 0)
                              (balance-weight-mode 1)
                              (setq unread-command-events
                                    (append (kbd (concat "H-" key)) nil)))
                          (if (commandp binding)
                              (call-interactively binding)))))))))
          100)




(resolve init-multi-vterm)
