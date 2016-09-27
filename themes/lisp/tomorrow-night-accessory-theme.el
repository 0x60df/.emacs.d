
;;;; tomorrow-accessory-theme.el


(deftheme tomorrow-night-accessory)

;;; mode-line

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(mode-line-buffer-identification-face
   ((t :inherit (font-lock-keyword-face mode-line-buffer-id)
       :weight bold)))
 '(mode-line-vc-mode-face ((t :inherit font-lock-variable-name-face)))
 '(mode-line-mode-name-face ((t :inherit font-lock-type-face)))
 '(mode-line-which-func-mode-face ((t :inherit font-lock-function-name-face))))


;;; term-mode

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(term-color-black ((t :foreground ,background :background ,background)))
  `(term-color-blue ((t :foreground ,blue :background ,blue)))
  `(term-color-cyan ((t :foreground ,aqua :background ,aqua)))
  `(term-color-green ((t :foreground ,green :background ,green)))
  `(term-color-magenta ((t :foreground ,purple :background ,purple)))
  `(term-color-red ((t :foreground ,red :background ,red)))
  `(term-color-white ((t :foreground ,foreground :background ,foreground)))
  `(term-color-yellow ((t :foreground ,yellow :background ,yellow)))))


;;; popup

(when (featurep 'popup)
  (custom-theme-set-variables
   'tomorrow-night-accessory
   '(popup-isearch-cursor-color-adjuster
     (lambda () (face-foreground 'isearch))))

  (custom-theme-set-faces
   'tomorrow-night-accessory
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
   'tomorrow-night-accessory
   '(ac-fuzzy-cursor-color-adjuster
     (lambda () (face-foreground 'warning))))

  (custom-theme-set-faces
   'tomorrow-night-accessory
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
  (color-theme-tomorrow--with-colors
   'night
   (custom-theme-set-faces
    'tomorrow-night-accessory
    `(git-gutter-fr:modified
      ((((type graphic))
        :inherit ediff-fine-diff-C
        :foreground ,background
        :inverse-video t)))
    `(git-gutter-fr:added
      ((((type graphic))
        :inherit ediff-fine-diff-B
        :foreground ,background
        :inverse-video t)))
    `(git-gutter-fr:deleted
      ((((type graphic))
        :inherit ediff-fine-diff-A
        :foreground ,background
        :inverse-video t))))))


;;; helm

(when (featurep 'helm)
  (custom-theme-set-faces
   'tomorrow-night-accessory
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
   'tomorrow-night-accessory
   '(ace-jump-face-foreground ((t :foreground "gold")))))


;;; helm swoop

(when (fboundp 'helm-swoop)
  (custom-theme-set-faces
   'tomorrow-night-accessory
   '(helm-swoop-line-number-face ((t :inherit shadow)))
   '(helm-swoop-target-line-block-face ((t :inherit secondary-selection)))
   '(helm-swoop-target-line-face ((t :inherit region)))
   '(helm-swoop-target-word-face ((t :inherit helm-match)))))


;;; evil

(when (featurep 'evil)

  (custom-theme-set-variables
   'tomorrow-night-accessory

   '(evil-emacs-state-cursor-adjuster
     (lambda () (if window-system
                    (color-theme-tomorrow--with-colors 'night red)
                  nil)))
   '(evil-normal-state-cursor-adjuster
     (lambda () (if window-system
                    (color-theme-tomorrow--with-colors 'night green)
                  nil)))
   '(evil-insert-state-cursor-adjuster
     (lambda ()
       (let ((painter
              (if window-system
                  (lambda (cursor)
                    (list (color-theme-tomorrow--with-colors 'night orange)
                          cursor))
                (symbol-function 'identity))))
         (funcall painter 'bar))))
   '(evil-visual-state-cursor-adjuster
     (lambda () (if window-system
                    (color-theme-tomorrow--with-colors 'night blue)
                  nil)))
   '(evil-replace-state-cursor-adjuster
     (lambda ()
       (let ((painter
              (if window-system
                  (lambda (cursor)
                    (list (color-theme-tomorrow--with-colors 'night orange)
                          cursor))
                (symbol-function 'identity))))
         (funcall painter 'hbar))))
   '(evil-operator-state-cursor-adjuster
     (lambda () (if window-system
                    (color-theme-tomorrow--with-colors 'night yellow)
                  nil)))
   '(evil-motion-state-cursor-adjuster
     (lambda () (if window-system
                    (color-theme-tomorrow--with-colors 'night aqua)
                  nil)))

   ;; prohibit changing cursor while ac-menu is live
   '(evil-reasons-for-interruption-of-reflesh-cursor
     '((and (or (eq evil-state 'emacs)
                (eq evil-state 'insert)
                (eq evil-state 'replace))
            (ac-menu-live-p))))))


(provide-theme 'tomorrow-night-accessory)
