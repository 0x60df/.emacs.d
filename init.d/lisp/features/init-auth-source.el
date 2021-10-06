
;;;; init-auth-source.el


(premise init)
(premise custom)

(custom-set-variables
 `(auth-sources '((:source ,(concat user-emacs-directory ".authinfo.gpg"))))
 '(auth-source-save-behavior nil))


(resolve init-auth-source)
