
;;;; init-el-get.el


(premise init)
(premise custom)
(premise inst-el-get)

(with-eval-after-load 'el-get-list-packages
  (add-hook 'el-get-package-menu-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))


(resolve init-el-get)
