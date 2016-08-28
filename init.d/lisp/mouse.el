
;;;; mouse.el


(defadvice mouse-set-point (around set-mark-before-mouse-set-point)
  (push-mark) ad-do-it (pop-mark))

(ad-activate 'mouse-set-point)
