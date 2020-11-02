
;;;; init-quickrun.el


(premise init)
(premise inst-quickrun)

(eval-when-compile (require 'ruby-mode))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c q") #'quickrun))


(resolve init-quickrun)
