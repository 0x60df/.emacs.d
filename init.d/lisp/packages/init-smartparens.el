
;;;; smartparens.el


(premise init)
(premise custom)
(premise bindings)
(premise feature)
(premise inst-smartparens)

(lazy-autoload 'sp-splice-sexp "smartparens")
(lazy-autoload 'sp-rewrap-sexp "smartparens")
(declare-function sp-local-pair "smartparens")

(custom-set-variables
 '(sp-highlight-pair-overlay nil)
 '(sp-highlight-wrap-overlay nil)
 '(sp-highlight-wrap-tag-overlay nil))

(overriding-set-key (kbd "C-(") #'sp-splice-sexp)
(overriding-set-key (kbd "C-)") #'sp-rewrap-sexp)

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'smartparens)

            (mapc (lambda (mode)
                    (sp-local-pair mode "'" nil :actions nil)
                    (sp-local-pair mode "`" "'"
                                   :when '(sp-in-docstring-p
                                           sp-in-string-p
                                           sp-in-comment-p)))
                  '(emacs-lisp-mode lisp-interaction-mode))

            (smartparens-global-mode)))


(resolve init-smartparens)
