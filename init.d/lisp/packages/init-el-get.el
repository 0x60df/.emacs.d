
;;;; init-el-get.el


(premise init)
(premise custom)
(premise inst-el-get)

(custom-set-variables
 '(el-get-user-package-directory
   (concat user-emacs-directory "el-get-user/init-files")))


(resolve init-el-get)
