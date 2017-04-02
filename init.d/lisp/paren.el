
;;;; paren.el


(premise init)

(eval-when-compile (require 'paren))

(show-paren-mode 1)
(eval-after-load 'paren
  '(custom-set-variables '(show-paren-style 'mixed)))


(resolve paren)
