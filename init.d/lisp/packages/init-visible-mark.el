
;;;; init-visible-mark.el


(premise init)
(premise custom)
(premise inst-visible-mark)

(eval-when-compile (require 'visible-mark))

(declare-function global-visible-mark-mode "visible-mark")

(custom-set-variables
 '(visible-mark-max 1)
 '(visible-mark-inhibit-trailing-overlay t))

(defun visible-mark-overlays-set-priority (&rest args)
  "Advising function for `visible-mark-initialize-overlays'.
Set priority of `visible-mark-overlays'."
  (mapc (lambda (overlay)
          (overlay-put overlay 'priority 200))
        visible-mark-overlays))
(advice-add 'visible-mark-initialize-overlays
            :after #'visible-mark-overlays-set-priority)

(defun visible-mark-revert-face-on-cursor-after-move-overlays (&rest args)
  "Advising function for `visible-mark-move-overlays'.
If `overlay-start' of any `visible-mark-overlay' equal to
`point', put the face of the overlay as text property of the
`point'."
  (mapc (lambda (overlay)
          (if (eq (overlay-start overlay) (point))
              (overlay-put overlay 'face (get-text-property (point) 'face))))
        visible-mark-overlays))
(advice-add 'visible-mark-move-overlays
            :after #'visible-mark-revert-face-on-cursor-after-move-overlays)

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'visible-mark)
            (global-visible-mark-mode)))


(resolve init-visible-mark)
