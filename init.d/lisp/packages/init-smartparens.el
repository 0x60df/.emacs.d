
;;;; smartparens.el



;;; base

(premise init)
(premise inst-smartparens)

(require 'smartparens)

(smartparens-global-mode)
(custom-set-variables '(sp-highlight-pair-overlay nil)
                      '(sp-highlight-wrap-overlay nil)
                      '(sp-highlight-wrap-tag-overlay nil))
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)

;;; bindings

(global-set-key (kbd "C-(") 'sp-splice-sexp)
(global-set-key (kbd "C-)") 'sp-rewrap-sexp)


(resolve init-smartparens)
