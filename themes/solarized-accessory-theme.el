
;;;; solarized-accessory-theme.el


(deftheme solarized-accessory)

;;; auto-complete

(when (featurep 'auto-complete)
  (custom-theme-set-faces
   'solarized-accessory
   `(ac-candidate-face
     ((t :foreground ,(face-foreground 'default)
         :background ,(face-background 'default))))
   `(ac-selection-face
     ((t :foreground ,(face-background 'default)
         :background ,(face-foreground 'default))))
   `(ac-completion-face
     ((t :foreground ,(face-foreground 'font-lock-comment-face)
         :underline ,(face-foreground 'font-lock-comment-face))))
   `(popup-tip-face
     ((t :foreground ,(face-foreground 'mode-line)
         :background ,(face-background 'mode-line))))
   `(ac-yasnippet-candidate-face
     ((t :inherit ac-candidate-face
         :foreground ,(face-foreground 'font-lock-warning-face))))
   `(ac-yasnippet-selection-face
     ((t :inherit ac-selection-face)))
   `(ac-dictionary-candidate-face
     ((t :inherit ac-candidate-face
         :foreground ,(face-foreground 'font-lock-keyword-face))))
   `(ac-dictionary-selection-face
     ((t :inherit ac-selection-face)))
   `(ac-symbols-candidate-face
     ((t :inherit ac-candidate-face
         :foreground ,(face-foreground 'font-lock-constant-face))))
   `(ac-symbols-selection-face
     ((t :inherit ac-selection-face)))
   `(ac-variables-candidate-face
     ((t :inherit ac-candidate-face
         :foreground ,(face-foreground 'font-lock-variable-name-face))))
   `(ac-variables-selection-face
     ((t :inherit ac-selection-face)))
   `(ac-functions-candidate-face
     ((t :inherit ac-candidate-face
         :foreground ,(face-foreground 'font-lock-function-name-face))))
   `(ac-functions-selection-face
     ((t :inherit ac-selection-face)))))

(provide-theme 'solarized-accessory)
