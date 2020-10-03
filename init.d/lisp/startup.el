
;;;; startup.el


(premise init)
(premise custom)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil))


(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Welcome to GNU Emacs, one component of the GNU operating system.")))


(resolve startup)
