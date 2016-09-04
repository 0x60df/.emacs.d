
;;;; smartparens.el



;;; base

(smartparens-global-mode)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)

;;; bindings

(global-set-key (kbd "C-(") (lambda (arg)
                              (interactive "P")
                              (if arg
                                  (sp-backward-slurp-sexp)
                                (sp-forward-barf-sexp))))
(global-set-key (kbd "C-)") (lambda (arg)
                              (interactive "P")
                              (if arg
                                  (sp-backward-barf-sexp)
                                (sp-forward-slurp-sexp))))
(global-set-key (kbd "s-(") 'sp-splice-sexp)
(global-set-key (kbd "s-)") 'sp-rewrap-sexp)
