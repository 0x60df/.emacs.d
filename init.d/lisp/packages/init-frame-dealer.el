
;;;; init-frame-dealer.el


(premise init)
(premise inst-frame-dealer)

(require 'frame-dealer)
(custom-set-variables '(frame-dealer-dealing-rule 'frame-dealer-random))
(frame-dealer-mode 1)


(resolve init-frame-dealer)
