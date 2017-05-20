
;;;; init-el-get.el


(premise init)
(premise inst-el-get)

(eval-after-load 'el-get-recipes
  '(custom-set-variables
    '(el-get-user-package-directory
      (concat user-emacs-directory "el-get-user/init-files"))))


(resolve init-el-get)
