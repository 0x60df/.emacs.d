
;;;; init-replace-colorthemes.el


(premise init)
(premise inst-replace-colorthemes)

(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "el-get/replace-colorthemes"))


(resolve init-replace-colorthemes)
