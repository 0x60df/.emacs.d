
;;;; init-visible-mark.el


(require 'visible-mark)
(custom-set-variables '(visible-mark-max 1)
                      '(visible-mark-inhibit-trailing-overlay t))
(defadvice visible-mark-initialize-overlays (after set-priority)
  (mapc (lambda (overlay)
          (overlay-put overlay 'priority 200))
        visible-mark-overlays))
(ad-activate 'visible-mark-initialize-overlays)

(defadvice visible-mark-move-overlays (after revert-face-on-cursor)
  (mapc (lambda (overlay)
          (if (eq (overlay-start overlay) (point))
              (overlay-put overlay 'face
                           (get-text-property (point) 'face))))
        visible-mark-overlays))
(ad-activate 'visible-mark-move-overlays)

(global-visible-mark-mode 1)
