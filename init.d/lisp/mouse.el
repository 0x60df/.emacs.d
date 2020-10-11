
;;;; mouse.el


(premise init)
(premise custom)

(custom-set-variables
 '(mouse-highlight nil))

(defun push-mark-at-point (&rest args)
  "Push mark at point.
Any ARGS are omitted."
  (push-mark))

(advice-add 'mouse-drag-region :before #'push-mark-at-point)


(resolve mouse)
