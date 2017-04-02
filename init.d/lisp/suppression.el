
;;;; suppresion.el


(premise init)

(custom-set-variables '(ring-bell-function 'ignore))

(eval-after-load 'advice
  '(custom-set-variables '(ad-redefinition-action 'accept)))


(resolve suppression)
