
;;;; suppresion.el


(premise init)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(custom-set-variables
 '(ring-bell-function 'ignore)
 '(echo-keystrokes 0)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ad-redefinition-action 'accept))

(tooltip-mode 0)
(setq show-help-function nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Welcome to GNU Emacs, one component of the GNU operating system.")))


(resolve suppression)
