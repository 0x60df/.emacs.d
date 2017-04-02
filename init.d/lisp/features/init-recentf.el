
;;;; init-recentf.el


(premise init)

(eval-after-load 'recentf
  '(custom-set-variables '(recentf-max-saved-items 1000)))


(resolve init-recentf)
