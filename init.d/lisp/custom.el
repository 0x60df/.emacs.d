
;;;; custom.el


(premise init)

(defgroup user nil "Group for user customization" :group 'emacs)

(custom-set-variables
 '(custom-file (concat user-emacs-directory "custom.el"))
 '(custom-magic-show-button t))


(resolve custom)
