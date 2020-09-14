
;;;; tomorrow-night-patch-theme.el


(deftheme tomorrow-night-patch)

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-patch
  `(highlight ((((type graphic)) :distant-foreground ,background)))
  `(lazy-highlight
    ((t (:foreground ,aqua :background ,background :inverse-video t))))))

(provide-theme 'tomorrow-night-patch)
