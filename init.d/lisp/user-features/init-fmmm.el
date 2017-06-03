
;;;; init-fmmm.el

(premise init)

(custom-set-variables '(fmmm-minor-mode-variable-alist
                        '((auto-fill-mode . auto-fill-function))))
(fmmm-declare-major-mode 'shell-script-mode
                         'ruby-mode)

(resolve fmmm)
