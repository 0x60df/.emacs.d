
;;;; solarized-accessory-theme.el


(deftheme solarized-accessory)

;;;popup

(when (featurep 'popup)
  (custom-theme-set-variables
   'solarized-accessory
   '(popup-isearch-cursor-color (face-foreground 'isearch)))

  (custom-theme-set-faces
   'solarized-accessory
   '(popup-face ((t :inherit default)))
   '(popup-isearch-match ((t :inherit lazy-highlight)))
   '(popup-menu-mouse-face ((t :inherit popup-menu-selection-face)))
   '(popup-menu-selection-face ((t :inherit region)))
   '(popup-scroll-bar-background-face ((t :inherit mode-line-inactive)))
   '(popup-scroll-bar-foreground-face ((t :inherit mode-line-inactive
                                          :inverse-video t)))
   '(popup-summary-face ((t :inherit shadow)))
   '(popup-tip-face ((t :inherit mode-line)))))


;;; auto-complete

(when (featurep 'auto-complete)
  (custom-theme-set-faces
   'solarized-accessory
   '(ac-gtags-candidate-face
     ((t :inherit (font-lock-function-name-face ac-candidate-face))))
   '(ac-gtags-selection-face
     ((t :inherit (font-lock-function-name-face ac-selection-face))))
   '(ac-yasnippet-candidate-face
     ((t :inherit (font-lock-warning-face ac-candidate-face))))
   '(ac-yasnippet-selection-face
     ((t :inherit (font-lock-warning-face ac-selection-face))))
   '(ac-dictionary-candidate-face
     ((t :inherit (font-lock-keyword-face ac-candidate-face))))
   '(ac-dictionary-selection-face
     ((t :inherit (font-lock-keyword-face ac-selection-face))))
   '(ac-symbols-candidate-face
     ((t :inherit (font-lock-constant-face ac-candidate-face))))
   '(ac-symbols-selection-face
     ((t :inherit (font-lock-constant-face ac-selection-face))))
   '(ac-variables-candidate-face
     ((t :inherit (font-lock-variable-name-face ac-candidate-face))))
   '(ac-variables-selection-face
     ((t :inherit (font-lock-variable-name-face ac-selection-face))))
   '(ac-functions-candidate-face
     ((t :inherit (font-lock-function-name-face ac-candidate-face))))
   '(ac-functions-selection-face
     ((t :inherit (font-lock-function-name-face ac-selection-face))))))


;;; helm

(when (featurep 'helm)
  (custom-theme-set-faces
   'solarized-accessory
   '(helm-ff-dotted-directory ((t :inherit (dired-ignored italic))))
   '(helm-ff-dotted-symlink-directory ((t :inherit (dired-symlink italic))))))


(provide-theme 'solarized-accessory)
