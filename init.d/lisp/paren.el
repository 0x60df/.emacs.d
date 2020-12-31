
;;;; paren.el


(premise init)
(premise custom)

(show-paren-mode)
(custom-set-variables
 '(show-paren-style 'mixed)
 '(show-paren-priority 0))


(resolve paren)
