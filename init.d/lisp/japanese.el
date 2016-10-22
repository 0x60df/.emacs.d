
;;;; japanese.el


(mapc (lambda (mode)
        (font-lock-add-keywords mode '(("　" . 'trailing-whitespace))))
      '(nil c-mode emacs-lisp-mode))
