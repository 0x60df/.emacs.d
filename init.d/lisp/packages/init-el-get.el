
;;;; init-el-get.el


(premise init)
(premise custom)
(premise inst-el-get)

(custom-set-variables
 '(el-get-user-package-directory
   (concat user-emacs-directory "el-get-user/init-files")))

(with-eval-after-load 'el-get-list-packages
  (add-hook 'el-get-package-menu-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))


(resolve init-el-get)
