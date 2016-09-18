
;;;; japanese.el


(font-lock-add-keywords nil '(("　" . 'trailing-whitespace)))
(font-lock-add-keywords 'c-mode '(("　" . 'trailing-whitespace)))
(font-lock-add-keywords 'emacs-lisp-mode '(("　" . 'trailing-whitespace)))
