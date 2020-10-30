
;;;; init-dired.el


(premise init)
(premise custom)

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always))


(resolve init-dired)
