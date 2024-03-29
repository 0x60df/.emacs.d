
;;;; yester-accessory-theme.el


(deftheme yester-accessory)

(let ((class '((class color) (min-colors 89))))
  (yester-theme-set-faces
   'yester-accessory

   ;; Mode-line
   `(mode-line-highlight
     (yester-face-spec ',class :foreground green :weight 'bold :slant 'italic))
   `(mode-line-emphasis
     (yester-face-spec ',class :foreground green :weight 'bold :slant 'normal))
   `(mode-line-mode-name (yester-face-spec ',class
                           (night (nil :foreground yellow)
                                  (moonlight :foreground aqua :weight 'bold))
                           (day (nil :foreground blue :weight 'bold)
                                (sunlight :foreground red))))
   `(mode-line-warning (yester-face-spec ',class
                         (night :foreground red)
                         (day (nil :foreground red)
                              (sunlight :foreground red :weight 'bold))))
   `(mode-line-transform '((,class (:slant italic))))



   ;; VC
   `(vc-state-base (yester-face-spec ',class :foreground orange))
   `(vc-conflict-state (yester-face-spec ',class :foreground red))
   `(vc-edited-state (yester-face-spec ',class :foreground orange))
   `(vc-locally-added-state (yester-face-spec ',class :foreground orange))
   `(vc-locked-state (yester-face-spec ',class :foreground orange))
   `(vc-missing-state (yester-face-spec ',class :foreground orange))
   `(vc-needs-update-state (yester-face-spec ',class :foreground orange))
   `(vc-removed-state (yester-face-spec ',class :foreground orange))
   `(vc-up-to-date-state (yester-face-spec ',class :foreground green))



   ;; Risky
   `(risky-yes-or-no-p-prefix (yester-face-spec ',class :foreground red))



   ;; Sdired
   `(sdired-group (yester-face-spec ',class :foreground comment :slant 'italic))
   `(sdired-key (yester-face-spec ',class :foreground aqua :weight 'bold))



   ;; Auto-complete
   `(ac-dictionary-candidate-face
     '((,class :inherit (font-lock-keyword-face ac-candidate-face))))
   `(ac-dictionary-selection-face
     '((,class :inherit (font-lock-keyword-face ac-selection-face))))
   `(ac-symbols-candidate-face
     '((,class :inherit (font-lock-builtin-face ac-candidate-face))))
   `(ac-symbols-selection-face
     '((,class :inherit (font-lock-builtin-face ac-selection-face))))
   `(ac-variables-candidate-face
     '((,class :inherit (font-lock-variable-name-face ac-candidate-face))))
   `(ac-variables-selection-face
     '((,class :inherit (font-lock-variable-name-face ac-selection-face))))
   `(ac-functions-candidate-face
     '((,class :inherit (font-lock-function-name-face ac-candidate-face))))
   `(ac-functions-selection-face
     '((,class :inherit (font-lock-function-name-face ac-selection-face))))



   ;; Magit
   `(magit-blame-margin-body (yester-face-spec ',class
                               :weight 'normal
                               :slant 'italic
                               :foreground comment
                               :background background
                               :underline nil
                               :inverse-video nil))



   ;; Evil
   `(evil-normal-state-tag
     (yester-face-spec ',class :foreground green :weight 'bold))
   `(evil-insert-state-tag
     (yester-face-spec ',class :foreground green :weight 'bold))
   `(evil-replace-state-tag
     (yester-face-spec ',class :foreground green :weight 'bold))
   `(evil-operator-state-tag
     (yester-face-spec ',class :foreground green :weight 'bold))
   `(evil-visual-state-tag
     (yester-face-spec ',class :foreground green :weight 'bold))
   `(evil-motion-state-tag
     (yester-face-spec ',class :foreground comment :weight 'bold))))



(yester-theme-set-variables
 'yester-accessory

 ;; Undo
 `(undo-turning-point-cursor-color (yester-symbol-exp yellow))

 ;; Balance
 `(balance-mode-active-cursor-color (yester-symbol-exp purple))
 `(balance-mode-semi-active-cursor-color (yester-symbol-exp aqua))
 `(balance-mode-inactive-cursor-color (yester-symbol-exp red))

 ;; Calfw
 `(calfw-org-source-period-bgcolor (yester-symbol-exp background))
 `(calfw-org-source-period-fgcolor (yester-symbol-exp foreground))

 ;; Gnus
 `(gnus-mode-line-image-color (yester-symbol-exp purple))

 ;; Company
 `(company-search-cursor-color (yester-symbol-exp yellow))
 `(company-search-fail-cursor-color (yester-symbol-exp orange))

 ;; Multiple-cursors
 `(mc-mode-cursor-color (yester-symbol-exp orange))
 `(mc-mode-with-balance-mode-cursor-color (yester-symbol-exp yellow)))

(provide-theme 'yester-accessory)
