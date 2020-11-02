
;;;; init-page-break-lines.el


(premise init)
(premise custom)
(premise inst-page-break-lines)

(custom-set-variables
 '(page-break-lines-lighter ""))

(add-hook 'emacs-startup-hook #'global-page-break-lines-mode)


(resolve init-page-break-lines)
