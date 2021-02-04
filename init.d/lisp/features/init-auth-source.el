
;;;; init-auth-source.el


(premise init)
(premise custom)

(custom-set-variables
 `(auth-sources '((:source ,(concat user-emacs-directory ".authinfo.gpg")))))


(resolve init-auth-source)
