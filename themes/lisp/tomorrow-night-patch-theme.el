
;;;; tomorrow-night-patch-theme.el


(deftheme tomorrow-night-patch)

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-patch
  `(highlight ((((type graphic)) :distant-foreground ,background)))))

(provide-theme 'tomorrow-night-patch)
