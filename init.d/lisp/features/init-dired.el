
;;;; init-dired.el


(premise init)

(eval-after-load 'dired
  '(progn
     (custom-set-variables '(dired-dwim-target t)
                           '(dired-recursive-copies 'always))))


(resolve init-dired)
