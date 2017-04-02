
;;;; insurance.el


(premise init)

(custom-set-variables
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup")))
 '(backup-by-copying t)
 '(version-control t)
 '(kept-new-versions 5)
 '(kept-old-versions 1)
 '(delete-old-versions t)
 '(delete-auto-save-files t))


(resolve insurance)
