
;;;; paren.el


(premise init)
(premise custom)

(eval-when-compile (require 'paren))

(show-paren-mode 1)
(custom-set-variables
 '(show-paren-style 'mixed))


(resolve paren)
