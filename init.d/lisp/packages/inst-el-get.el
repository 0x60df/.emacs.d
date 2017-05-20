
;;;; inst-el-get.el


(premise init)

(eval-and-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "https://raw.githubusercontent.com/"
          "dimitri/el-get/master/el-get-install.el"))
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (add-to-list 'el-get-recipe-path
               (concat user-emacs-directory "el-get-user/recipes")))


(resolve inst-el-get)
