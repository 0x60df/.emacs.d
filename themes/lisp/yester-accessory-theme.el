
;;;; yester-accessory-theme.el


(deftheme yester-accessory)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'yester-accessory

   ;; Mode-line
   `(mode-line-highlight (,@(yester-whole-face-spec
                             class `(:foreground ,green
                                                 :weight bold
                                                 :slant italic))))
   `(mode-line-emphasis (,@(yester-whole-face-spec
                            class `(:foreground ,green
                                                :weight bold
                                                :slant normal))))
   `(mode-line-mode-name
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground yellow)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground red)))
               (t (yester-let-colors day
                    (list :foreground blue :weight 'bold)))))))
   `(mode-line-warning
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground red)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground red :weight 'bold)))
               (t (yester-let-colors day (list :foreground red)))))))
   `(mode-line-transform ((,class (:slant italic))))



   ;; VC
   `(vc-state-base (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-conflict-state (,@(yester-whole-face-spec class :foreground red)))
   `(vc-edited-state (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-locally-added-state
     (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-locked-state (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-missing-state (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-needs-update-state
     (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-removed-state (,@(yester-whole-face-spec class :foreground orange)))
   `(vc-up-to-date-state (,@(yester-whole-face-spec class :foreground green)))



   ;; Risky
   `(risky-yes-or-no-p-prefix
     (,@(yester-whole-face-spec class :foreground red)))



   ;; Sdired
   `(sdired-group (,@(yester-whole-face-spec class
                       :foreground comment :slant 'italic)))
   `(sdired-key (,@(yester-whole-face-spec class
                     :foreground aqua :weight 'bold)))



   ;; Company
   `(company-tooltip-yasnippet
     (,@(yester-whole-face-spec class :foreground red)))
   `(company-tooltip-dabbrev-code
     (,@(yester-whole-face-spec class :foreground purple)))



   ;; Auto-complete
   `(ac-dictionary-candidate-face
     ((,class :inherit (font-lock-keyword-face ac-candidate-face))))
   `(ac-dictionary-selection-face
     ((,class :inherit (font-lock-keyword-face ac-selection-face))))
   `(ac-symbols-candidate-face
     ((,class :inherit (font-lock-builtin-face ac-candidate-face))))
   `(ac-symbols-selection-face
     ((,class :inherit (font-lock-builtin-face ac-selection-face))))
   `(ac-variables-candidate-face
     ((,class :inherit (font-lock-variable-name-face ac-candidate-face))))
   `(ac-variables-selection-face
     ((,class :inherit (font-lock-variable-name-face ac-selection-face))))
   `(ac-functions-candidate-face
     ((,class :inherit (font-lock-function-name-face ac-candidate-face))))
   `(ac-functions-selection-face
     ((,class :inherit (font-lock-function-name-face ac-selection-face))))



   ;; Magit
   `(magit-blame-margin-body (,@(yester-whole-face-spec class
                                  :weight 'normal
                                  :slant 'italic
                                  :foreground comment
                                  :background background
                                  :underline nil
                                  :inverse-video nil)))



   ;; Evil
   `(evil-normal-state-tag
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(evil-insert-state-tag
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(evil-replace-state-tag
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(evil-operator-state-tag
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(evil-visual-state-tag
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(evil-motion-state-tag
     (,@(yester-whole-face-spec class :foreground comment :weight 'bold)))))



(custom-theme-set-variables
 'yester-accessory

 ;; Calfw
 `(calfw-org-source-period-bgcolor ,(yester-whole-symbol-exp background))
 `(calfw-org-source-period-fgcolor ,(yester-whole-symbol-exp foreground))

 ;; Gnus
 `(gnus-mode-line-image-color ,(yester-whole-symbol-exp purple))

 ;; Company
 `(company-search-cursor-color ,(yester-whole-symbol-exp yellow))
 `(company-search-fail-cursor-color ,(yester-whole-symbol-exp orange))

 ;; Multiple-cursors
 `(mc-mode-cursor-color ,(yester-whole-symbol-exp orange)))

(provide-theme 'yester-accessory)
