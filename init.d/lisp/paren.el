
;;;; paren.el


(premise init)

(eval-when-compile (require 'paren))

(show-paren-mode 1)
(custom-set-variables
 '(show-paren-style 'mixed))


(resolve paren)
