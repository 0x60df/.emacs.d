
;;;; tomorrow-accessory-theme.el


(deftheme tomorrow-accessory)

;;; mode-line

(custom-theme-set-faces
 'tomorrow-accessory
 '(mode-line-buffer-identification-face
   ((t :inherit (font-lock-keyword-face mode-line-buffer-id)
       :weight bold)))
 '(mode-line-vc-mode-face ((t :inherit font-lock-variable-name-face)))
 '(mode-line-mode-name-face ((t :inherit font-lock-type-face)))
 '(mode-line-which-func-mode-face ((t :inherit font-lock-function-name-face))))


;;; popup

(when (featurep 'popup)
  (custom-theme-set-variables
   'tomorrow-accessory
   '(popup-isearch-cursor-color (face-foreground 'isearch)))

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
  (custom-theme-set-variables
   'tomorrow-accessory
   '(ac-fuzzy-cursor-color (face-foreground 'warning)))

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


;;; git-getter-fringe

(when (featurep 'git-gutter-fringe)
  (require 'ediff)
  (custom-theme-set-faces
   'tomorrow-accessory
   `(git-gutter-fr:modified
     ((t :foreground ,(face-background 'ediff-fine-diff-C))))
   `(git-gutter-fr:added
     ((t :foreground ,(face-background 'ediff-fine-diff-B))))
   `(git-gutter-fr:deleted
     ((t :foreground ,(face-background 'ediff-fine-diff-A))))))


;;; helm

(when (featurep 'helm)
  (custom-theme-set-faces
   'tomorrow-accessory
   '(helm-source-header ((t :inherit (info-title-1 hl-line))))
   '(helm-action ((t :inherit default)))
   '(helm-candidate-number ((t :inherit isearch)))
   '(helm-header-line-left-margin ((t :inherit dired-header :inverse-video t)))
   '(helm-selection ((t :inherit region)))
   '(helm-separator ((t :inherit error)))
   '(helm-visible-mark ((t :inherit highlight)))
   '(helm-buffer-directory ((t :inherit dired-directory)))
   '(helm-buffer-file ((t :inherit default)))
   '(helm-buffer-not-saved ((t :inherit warning)))
   '(helm-buffer-saved-out ((t :inherit error)))
   '(helm-ff-directory ((t :inherit dired-directory)))
   '(helm-ff-dirs ((t :inherit dired-directory :inverse-video t)))
   '(helm-ff-dotted-directory ((t :inherit (dired-ignored italic))))
   '(helm-ff-dotted-symlink-directory ((t :inherit (dired-symlink italic))))
   '(helm-ff-executable ((t :inherit success)))
   '(helm-ff-file ((t :inherit default)))
   '(helm-ff-invalid-symlink ((t :inherit dired-warning)))
   '(helm-ff-prefix ((t :inherit isearch)))
   '(helm-ff-symlink ((t :inherit dired-symlink)))
   '(helm-grep-file ((t :inherit font-lock-keyword-face)))
   '(helm-grep-finish ((t :inherit compilation-mode-line-exit)))
   '(helm-grep-lineno ((t :inherit font-lock-variable-name-face)))
   '(helm-moccur-buffer ((t :inherit font-lock-builtin-face)))))


;;; ace-jump-mode

(when (fboundp 'ace-jump-mode)
  (custom-theme-set-faces
   'tomorrow-accessory
   '(ace-jump-face-foreground ((t :foreground "gold")))))


;;; helm swoop

(when (fboundp 'helm-swoop)
  (custom-theme-set-faces
   'tomorrow-accessory
   '(helm-swoop-line-number-face ((t :inherit shadow)))
   '(helm-swoop-target-line-block-face ((t :inherit secondary-selection)))
   '(helm-swoop-target-line-face ((t :inherit region)))
   '(helm-swoop-target-word-face ((t :inherit helm-match)))))


;;; evil

(when (featurep 'evil)

  (custom-theme-set-variables
   'tomorrow-accessory

   '(evil-emacs-state-cursor (face-background 'cursor))
   '(evil-normal-state-cursor (face-foreground 'font-lock-string-face))
   '(evil-insert-state-cursor `(,(face-foreground 'font-lock-variable-name-face)
                                bar))
   '(evil-visual-state-cursor (face-foreground 'font-lock-function-name-face))
   '(evil-replace-state-cursor `(,(face-foreground
                                   'font-lock-variable-name-face)
                                 hbar))
   '(evil-operator-state-cursor (face-foreground 'font-lock-type-face))
   '(evil-motion-state-cursor (face-foreground 'font-lock-builtin-face))

   ;; prohibit changing cursor while ac-menu is live
   '(evil-reasons-for-interruption-of-reflesh-cursor
     '((and (or (eq evil-state 'emacs)
                (eq evil-state 'insert)
                (eq evil-state 'replace))
            (ac-menu-live-p))))))


(provide-theme 'tomorrow-accessory)
