
;;;; init-quickrun.el



;;; base

(premise init)
(premise inst-quickrun)

(eval-when-compile
  (require 'ruby-mode))


;;; bindings

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-ccq" 'quickrun)))


(resolve init-quickrun)
