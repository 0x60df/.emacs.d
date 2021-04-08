
;;;; init-el-get.el


(premise init)
(premise custom)
(premise inst-el-get)

(with-eval-after-load 'el-get-recipes
  (add-to-list 'el-get-recipe-path
               (concat user-emacs-directory "el-get-user/site-recipes")))

(with-eval-after-load 'el-get-list-packages
  (add-hook 'el-get-package-menu-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))


(resolve init-el-get)
