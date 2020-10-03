
;;;; suppresion.el


(premise init)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(custom-set-variables
 '(ring-bell-function 'ignore)
 '(echo-keystrokes 0)
 '(ad-redefinition-action 'accept))

(tooltip-mode 0)
(setq show-help-function nil)


(resolve suppression)
