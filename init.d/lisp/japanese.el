
;;;; japanese.el


(font-lock-add-keywords nil '(("　" . 'trailing-whitespace)))
(font-lock-add-keywords 'c-mode '(("　" . 'trailing-whitespace)))
