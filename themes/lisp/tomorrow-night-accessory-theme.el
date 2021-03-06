
;;;; tomorrow-night-accessory-theme.el


(deftheme tomorrow-night-accessory)

;;; basic face

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(escape-glyph ((t :foreground ,red)))
  `(homoglyph ((t :foreground ,red :weight bold)))
  `(nobreak-space ((t :box ,red)))
  `(nobreak-hyphen ((t :foreground ,red :weight bold)))))


;;; mode-line

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(mode-line-buffer-id ((t :weight bold)))
 '(mode-line-highlight ((t :inherit font-lock-string-face
                           :weight bold
                           :slant italic
                           :foreground nil)))
 '(mode-line-emphasis ((t :inherit font-lock-string-face
                          :weight bold
                          :slant normal
                          :foreground nil)))
 '(mode-line-vc-mode ((t :inherit font-lock-variable-name-face)))
 '(mode-line-mode-name ((t :inherit font-lock-type-face)))
 '(which-func ((t :weight normal)))
 '(mode-line-warning ((t :inherit font-lock-warning-face
                         :weight normal)))
 '(mode-line-transform ((t :slant italic))))


;;; risky
(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(risky-yes-or-no-p-prefix
    ((t :foreground ,red)))))


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


;;; org-mode

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(org-drawer ((t :foreground ,comment)))
  `(org-time-grid ((t :foreground ,yellow)))))


;;; hi-lock

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(hi-blue ((t :foreground ,blue :inverse-video t)))
  `(hi-blue-b ((t :foreground ,blue :weight bold)))
  `(hi-green ((t :foreground ,green :inverse-video t)))
  `(hi-green-b ((t :foreground ,green :weight bold)))
  `(hi-pink ((t :foreground ,red :inverse-video t)))
  `(hi-red-b ((t :foreground ,red :weight bold)))
  `(hi-yellow ((t :foreground ,yellow :inverse-video t)))))

;;; eww

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(eww-invalid-certificate ((t :inherit error)))
 '(eww-valid-certificate ((t :inherit success))))


;;; volatile-highlights

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(vhl/default-face ((t :inherit region))))


;;; wgrep

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(wgrep-face ((t :foreground ,aqua :inverse-video t)))
  `(wgrep-done-face ((t :foreground ,blue)))))


;;; popup

(custom-theme-set-variables
 'tomorrow-night-accessory
 `(popup-isearch-cursor-color
   ,(color-theme-tomorrow--with-colors 'night-eighties yellow)))

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(popup-face ((t :inherit default)))
 '(popup-isearch-match ((t :inherit (lazy-highlight default))))
 '(popup-menu-mouse-face ((t :inherit popup-menu-selection-face)))
 '(popup-menu-selection-face ((t :inherit (region default))))
 '(popup-scroll-bar-background-face ((t :inherit
                                        (mode-line-inactive default))))
 '(popup-scroll-bar-foreground-face ((t :inherit
                                        (mode-line-inactive default)
                                        :inverse-video t)))
 '(popup-summary-face ((t :inherit (shadow default))))
 '(popup-tip-face ((t :inherit (mode-line default)))))


;;; auto-complete

(custom-theme-set-variables
 'tomorrow-night-accessory
 `(ac-fuzzy-cursor-color
   ,(color-theme-tomorrow--with-colors 'night-eighties orange)))

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(ac-completion-face
   ((t :inherit shadow :underline t)))
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
   ((t :inherit (font-lock-function-name-face ac-selection-face)))))


;; diff

(defface tn-diff-green
  '((t :background "#335533"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-red
  '((t :background "#553333"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-yellow
  '((t :background "#524A32"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-refine-green
  `((t :foreground ,(color-theme-tomorrow--with-colors 'night-eighties green)
       :background "#048900"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-refine-red
  `((t :foreground "#F5A2A6"
       :background ,(color-theme-tomorrow--with-colors 'day red)))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-refine-yellow
  '((t :foreground "#FFE265"
       :background "#D6A400"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-indicator-green
  '((t :foreground "#048900"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-indicator-red
  `((t :foreground ,(color-theme-tomorrow--with-colors 'day red)))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")
(defface tn-diff-indicator-yellow
  '((t :foreground "#D6A400"))
  "Base face for the context of diff fit with `tomorrow-night-theme'.")

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(diff-added ((t :foreground nil
                  :inherit (tn-diff-green diff-changed))))
 '(diff-changed ((t :foreground nil
                    :inherit tn-diff-yellow)))
 '(diff-removed ((t :foreground nil
                    :inherit (tn-diff-red diff-changed ))))
 '(diff-refine-added ((t :inherit (tn-diff-refine-green diff-refine-changed))))
 '(diff-refine-changed ((t :inherit tn-diff-refine-yellow)))
 '(diff-refine-removed ((t :inherit (tn-diff-refine-red diff-refine-changed))))
 '(diff-indicator-added ((t :inherit (tn-diff-indicator-green diff-added))))
 '(diff-indicator-changed ((t :inherit (tn-diff-indicator-yellow
                                        diff-changed))))
 '(diff-indicator-removed ((t :inherit (tn-diff-indicator-red diff-removed)))))


;;; ediff

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(ediff-current-diff-C ((t :inherit tn-diff-yellow)))
 '(ediff-fine-diff-A ((t :inherit tn-diff-refine-red)))
 '(ediff-fine-diff-B ((t :inherit tn-diff-refine-green)))
 '(ediff-fine-diff-C ((t :inherit tn-diff-refine-yellow)))
 '(ediff-even-diff-C
   ((t :foreground nil
       :background nil
       :inverse-video t)))
 `(ediff-odd-diff-C
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night comment)
       :background nil
       :inverse-video t)))
 '(ediff-even-diff-Ancestor
   ((t :foreground nil
       :background nil
       :inverse-video t)))
 `(ediff-odd-diff-Ancestor
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night comment)
       :background nil
       :inverse-video t))))

;;; smerge

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(smerge-base ((t :inherit tn-diff-yellow)))
 '(smerge-lower ((t :inherit tn-diff-green)))
 '(smerge-refined-added ((t :inherit tn-diff-refine-green)))
 '(smerge-refined-removed ((t :inherit tn-diff-refine-red)))
 '(smerge-upper ((t :inherit tn-diff-red))))


;;; git-getter-fringe

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(git-gutter-fr:modified ((t :inherit tn-diff-indicator-yellow)))
 '(git-gutter-fr:added ((t :inherit tn-diff-indicator-green)))
 '(git-gutter-fr:deleted ((t :inherit tn-diff-indicator-red))))


;;; helm

(custom-theme-set-faces
 'tomorrow-night-accessory
 `(helm-M-x-key
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night orange)
       :underline t)))
 `(helm-source-header ((t :weight bold
                          :height 1.4
                          :inherit (variable-pitch hl-line))))
 '(helm-action ((t :inherit default)))
 `(helm-candidate-number
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night yellow))))
 '(helm-header-line-left-margin ((t :inherit dired-header :inverse-video t)))
 '(helm-selection ((t :inherit region)))
 '(helm-selection-line ((t :weight bold)))
 '(helm-separator ((t :inherit error)))
 `(helm-visible-mark ((t :underline
                         ,(color-theme-tomorrow--with-colors 'night yellow))))
 '(helm-buffer-directory ((t :inherit dired-directory)))
 '(helm-buffer-file ((t :inherit default)))
 '(helm-buffer-not-saved ((t :inherit warning)))
 '(helm-buffer-saved-out ((t :inherit error)))
 `(helm-buffer-modified
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night yellow))))
 `(helm-buffer-size ((t :inherit font-lock-comment-face)))
 `(helm-buffer-process
   ((t :foreground ,(color-theme-tomorrow--with-colors 'night aqua))))
 '(helm-ff-denied ((t :inherit (dired-warning italic))))
 '(helm-ff-directory ((t :inherit dired-directory)))
 '(helm-ff-dirs ((t :inherit dired-directory :inverse-video t)))
 '(helm-ff-dotted-directory ((t :inherit (helm-ff-directory italic))))
 '(helm-ff-dotted-symlink-directory ((t :inherit (helm-ff-symlink italic))))
 '(helm-ff-executable ((t :inherit success)))
 '(helm-ff-file ((t :inherit default)))
 '(helm-ff-file-extension ((t :inherit helm-ff-file)))
 '(helm-ff-invalid-symlink ((t :inherit dired-warning)))
 '(helm-ff-pipe ((t :inherit (dired-special))))
 `(helm-ff-prefix
   ((t :foreground ,(color-theme-tomorrow--with-colors 'day yellow)
       :weight bold)))
 '(helm-ff-socket ((t :inherit (dired-special italic))))
 '(helm-ff-suid ((t :inherit (dired-header))))
 '(helm-ff-symlink ((t :inherit dired-symlink)))
 '(helm-grep-file ((t :inherit font-lock-keyword-face)))
 '(helm-grep-finish ((t :inherit compilation-mode-line-exit)))
 '(helm-grep-lineno ((t :inherit font-lock-variable-name-face)))
 '(helm-locate-finish ((t :inherit success)))
 '(helm-match ((t :foreground "#ffd700")))
 '(helm-moccur-buffer ((t :inherit font-lock-builtin-face)))
 '(helm-mode-prefix ((t :inherit isearch))))


;;; ace-jump-mode

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(ace-jump-face-foreground ((t :foreground "#ffd700"))))


;;; helm swoop

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(helm-swoop-line-number-face ((t :inherit shadow)))
 '(helm-swoop-target-line-block-face ((t :inherit secondary-selection)))
 '(helm-swoop-target-line-face ((t :inherit region)))
 '(helm-swoop-target-word-face ((t :inherit helm-match))))


;;; visible-mark

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  '(visible-mark-face ((t :inverse-video t))))
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(show-paren-match ((t :background ,current-line
                         :foreground ,blue
                         :inverse-video t)))
  `(show-paren-mismatch ((t :background ,current-line
                            :foreground ,orange
                            :inverse-video t)))))


;;; magit

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(magit-branch-local ((t :foreground ,aqua)))
  `(magit-branch-remote ((t :foreground ,blue)))
  `(magit-hash ((t :foreground ,comment)))
  `(magit-log-author ((t :foreground ,orange)))
  `(magit-section-heading ((t :foreground ,yellow :weight bold)))
  `(magit-process-ng ((t :foreground ,red :inherit magit-section-heading)))
  `(magit-process-ok ((t :foreground ,green :inherit magit-section-heading)))))


;;; evil

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-variables
  'tomorrow-night-accessory

  `(evil-emacs-state-cursor ,red)
  `(evil-normal-state-cursor ,green)
  `(evil-insert-state-cursor '(bar ,green))
  `(evil-replace-state-cursor '(hbar ,green))
  `(evil-operator-state-cursor ,green)
  `(evil-visual-state-cursor ,green)
  `(evil-motion-state-cursor ,purple))

 (custom-theme-set-faces
  'tomorrow-night-accessory
  `(evil-ex-info ((t :foreground ,red :slant italic)))
  `(evil-ex-substitute-matches ((t :foreground ,red :underline t)))
  `(evil-normal-state-tag ((t :foreground ,green :weight bold)))
  `(evil-insert-state-tag ((t :foreground ,green :weight bold)))
  `(evil-replace-state-tag ((t :foreground ,green :weight bold)))
  `(evil-operator-state-tag ((t :foreground ,green :weight bold)))
  `(evil-visual-state-tag ((t :foreground ,green :weight bold)))
  `(evil-motion-state-tag ((t :foreground ,blue :weight bold)))))


;;; smartrep

(custom-theme-set-variables
 'tomorrow-night-accessory
 '(smartrep-mode-line-active-bg
   (color-theme-tomorrow--with-colors 'night selection)))


;;; visual-regexp

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(vr/group-0 ((t :inherit isearch)))
 '(vr/group-1 ((t :inherit isearch)))
 '(vr/group-2 ((t :inherit isearch)))
 '(vr/match-0 ((t :inherit lazy-highlight)))
 '(vr/match-1 ((t :inherit lazy-highlight)))
 '(vr/match-separator-face ((t :inherit error :weight bold))))


;;; anzu

(color-theme-tomorrow--with-colors
 'night
 (custom-theme-set-faces
  'tomorrow-night-accessory
  '(anzu-match-1 ((t :inherit query-replace)))
  '(anzu-match-2 ((t :inherit query-replace)))
  '(anzu-match-3 ((t :inherit query-replace)))
  `(anzu-mode-line ((t :foreground ,aqua)))
  `(anzu-mode-line-no-match ((t :foreground ,aqua)))
  '(anzu-replace-highlight ((t :inherit lazy-highlight)))
  '(anzu-replace-to ((t :foreground "#ffd700")))))


;;; color-moccur

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(moccur-face ((t :inherit match))))

;;; flex-isearch

(custom-theme-set-faces
 'tomorrow-night-accessory
 '(flex-isearch-message-prefix ((t :inherit font-lock-constant-face))))


;;; skk

(custom-theme-set-variables
 'tomorrow-night-accessory
 '(skk-use-color-cursor t)
 '(skk-cursor-abbrev-color (color-theme-tomorrow--with-colors 'night comment))
 '(skk-cursor-jisx0208-latin-color
   (color-theme-tomorrow--with-colors 'night comment))
 '(skk-cursor-katakana-color (color-theme-tomorrow--with-colors 'night aqua))
 '(skk-cursor-hiragana-color (color-theme-tomorrow--with-colors 'night blue))
 '(skk-cursor-jisx0201-color (color-theme-tomorrow--with-colors 'night comment))
 '(skk-cursor-latin-color (color-theme-tomorrow--with-colors 'night red)))

(custom-theme-set-faces
 'tomorrow-night-accessory
 `(skk-henkan-face-default
   ((((type graphic))
     :foreground ,(color-theme-tomorrow--with-colors 'night background)
     :background ,(color-theme-tomorrow--with-colors 'night aqua))))
 `(skk-prefix-hiragana-face
   ((((type graphic))
     :foreground ,(color-theme-tomorrow--with-colors 'night blue))))
 `(skk-prefix-jisx0201-face
   ((((type graphic))
     :foreground ,(color-theme-tomorrow--with-colors 'night comment))))
 `(skk-prefix-katakana-face
   ((((type graphic))
     :foreground ,(color-theme-tomorrow--with-colors 'night aqua)))))


(provide-theme 'tomorrow-night-accessory)
