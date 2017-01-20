
;;;; smartparens.el



;;; base

(smartparens-global-mode)
(custom-set-variables '(sp-highlight-pair-overlay nil)
                      '(sp-highlight-wrap-overlay nil)
                      '(sp-highlight-wrap-tag-overlay nil))
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)

;;; bindings

(global-set-key (kbd "C-(") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-)") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-(") (lambda (arg)
                              (interactive "P")
                              (if arg
                                  (insert-parentheses)
                                (sp-backward-slurp-sexp))))
(global-set-key (kbd "M-)") (lambda (arg)
                              (interactive "P")
                              (if arg
                                  (move-past-close-and-reindent)
                                (sp-backward-barf-sexp))))
(global-set-key (kbd "s-(") 'sp-splice-sexp)
(global-set-key (kbd "s-)") 'sp-rewrap-sexp)
