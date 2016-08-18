
;;;; tomorrow-accessory-theme.el


(deftheme tomorrow-accessory)

;;; mode-line

(custom-theme-set-faces
 'tomorrow-accessory
 `(mode-line-buffer-identification-face
   ((t :foreground ,(face-foreground 'font-lock-keyword-face)
       :weight bold)))
 `(mode-line-vc-mode-face
   ((t :foreground ,(face-foreground 'font-lock-variable-name-face))))
 `(mode-line-mode-name-face
   ((t :foreground ,(face-foreground 'font-lock-type-face))))
 `(mode-line-which-func-mode-face
   ((t :foreground ,(face-foreground 'font-lock-function-name-face)))))


;;; popup

(when (featurep 'popup)
  (custom-theme-set-faces
   'tomorrow-accessory
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
   'tomorrow-accessory
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

(provide-theme 'tomorrow-accessory)
