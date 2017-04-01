
;;;; mouse.el


(defadvice mouse-drag-region (before set-mark-before-mouse-drag-region)
  (push-mark))

(ad-activate 'mouse-drag-region)


(resolve mouse)
