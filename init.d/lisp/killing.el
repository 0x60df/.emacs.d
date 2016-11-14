
;;;; killing.el


(defadvice kill-region (around transient)
  (unless (and transient-mark-mode (not mark-active)) ad-do-it))

(ad-activate 'kill-region)

(defadvice kill-ring-save (around transient)
  (unless (and transient-mark-mode (not mark-active)) ad-do-it))

(ad-activate 'kill-ring-save)
