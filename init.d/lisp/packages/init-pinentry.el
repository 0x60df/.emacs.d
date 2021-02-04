
;;;; init-pinentry.el


(premise init)
(premise inst-pinentry)

(with-eval-after-load 'epa
  (pinentry-start))


(resolve init-pinentry)
