
;;;; suppresion.el


(premise init)

(custom-set-variables '(ring-bell-function 'ignore)
                      '(echo-keystrokes 0))

(eval-after-load 'advice
  '(custom-set-variables '(ad-redefinition-action 'accept)))


(resolve suppression)
