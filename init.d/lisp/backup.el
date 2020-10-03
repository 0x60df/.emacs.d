
;;;; backup.el


(premise init)
(premise custom)

(custom-set-variables
 '(backup-directory-alist `((".*" . ,(concat user-emacs-directory "backup"))))
 '(backup-by-copying t)
 '(version-control t)
 '(kept-new-versions 4)
 '(kept-old-versions 2)
 '(delete-old-versions t)
 '(delete-auto-save-files t))


(resolve backup)
