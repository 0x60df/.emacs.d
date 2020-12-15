;;; yester-theme.el --- Custom theme for faces derived from tomorrow theme

;; Copyright (C) 2020 0x60DF

;;; Commentary:

;; Theme settings

;; Color scheme is designed by Chris Kempson:
;; https://github.com/chriskempson/tomorrow-theme

;;; Code:

(require 'yester)

(deftheme yester "Custom theme for faces derived from tomorrow theme.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'yester

   ;; Basic faces
   `(default (,@(yester-whole-face-spec class
                  :foreground foreground :background background)))
   `(bold ((,class :weight bold)))
   `(italic ((,class :slant italic)))
   `(bold-italic ((,class :slant italic :weight bold)))
   `(underline ((,class :underline t)))

   `(shadow (,@(yester-whole-face-spec class :foreground comment)))
   `(link (,@(yester-whole-face-spec class :foreground blue)))
   `(link-visited (,@(yester-whole-face-spec class :foreground purple)))
   `(highlight (,@(yester-whole-face-spec class
                    :foreground green
                    :background background
                    :inverse-video t)))
   `(match (,@(yester-whole-face-spec class
                :foreground blue :background background :inverse-video t)))
   `(isearch (,@(yester-whole-face-spec class
                  :foreground yellow :background background :inverse-video t)))
   `(lazy-highlight (,@(yester-whole-face-spec class
                         :foreground aqua
                         :background background
                         :inverse-video t)))
   `(error (,@(yester-whole-face-spec class :foreground red)))
   `(warning (,@(yester-whole-face-spec class :foreground orange)))
   `(success (,@(yester-whole-face-spec class :foreground green)))



   ;; Font Lock
   `(font-lock-builtin-face
     (,@(yester-whole-face-spec class :foreground aqua)))
   `(font-lock-comment-delimiter-face
     (,@(yester-whole-face-spec class :foreground comment :slant 'italic)))
   `(font-lock-comment-face
     (,@(yester-whole-face-spec class :foreground comment :slant 'italic)))
   `(font-lock-constant-face
     (,@(yester-whole-face-spec class :foreground aqua)))
   `(font-lock-doc-face
     (,@(yester-whole-face-spec class :foreground comment)))
   `(font-lock-function-name-face
     (,@(yester-whole-face-spec class :foreground blue)))
   `(font-lock-keyword-face
     (,@(yester-whole-face-spec class :foreground purple)))
   `(font-lock-negation-char-face
     (,@(yester-whole-face-spec class :foreground green)))
   `(font-lock-preprocessor-face
     (,@(yester-whole-face-spec class :foreground purple)))
   `(font-lock-regexp-grouping-backslash
     (,@(yester-whole-face-spec class :foreground yellow)))
   `(font-lock-regexp-grouping-construct
     (,@(yester-whole-face-spec class :foreground purple)))
   `(font-lock-string-face
     (,@(yester-whole-face-spec class :foreground green)))
   `(font-lock-type-face
     (,@(yester-whole-face-spec class :foreground yellow)))
   `(font-lock-variable-name-face
     (,@(yester-whole-face-spec class :foreground orange)))
   `(font-lock-warning-face
     (,@(yester-whole-face-spec class :weight 'bold :foreground red)))



   ;; Standard Faces
   `(region (,@(yester-whole-face-spec class :background selection)))
   `(secondary-selection
     (,@(yester-whole-face-spec class :background current-line)))
   `(trailing-whitespace
     (,@(yester-whole-face-spec class :background red :foreground yellow)))
   `(escape-glyph (,@(yester-whole-face-spec class :foreground red)))
   `(homoglyph
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(nobreak-space (,@(yester-whole-face-spec class :box red)))
   `(nobreak-hyphen
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(mode-line (,@(yester-whole-face-spec class
                    :background selection :foreground foreground)))
   `(mode-line-inactive (,@(yester-whole-face-spec class
                             :background current-line :foreground foreground)))
   `(mode-line-highlight
     (,@(yester-whole-face-spec class :foreground purple :weight 'bold)))
   `(mode-line-buffer-id
     (,@(yester-whole-face-spec class :foreground purple :weight 'bold)))
   `(header-line
     (,@(yester-whole-face-spec class :foreground purple :inherit 'mode-line)))
   `(header-line-highlight
     (,@(yester-whole-face-spec class :foreground purple :weight 'bold)))
   `(tab-line (,@(yester-whole-face-spec class
                   :foreground foreground
                   :background current-line
                   :inherit 'variable-pitch)))
   `(minibuffer-prompt (,@(yester-whole-face-spec class :foreground blue)))
   `(fringe (,@(yester-whole-face-spec class :background current-line)))
   `(cursor (,@(yester-whole-face-spec class :background red)))
   `(tooltip (,@(yester-whole-face-spec class
                  :background selection :foreground foreground)))
   `(tool-bar (,@(yester-whole-face-spec class
                   :foreground foreground
                   :background current-line)))
   `(tab-bar (,@(yester-whole-face-spec class
                  :foreground foreground
                  :background current-line
                  :inherit 'variable-pitch)))



   ;; Complementary Faces for Basic and Standard Faces
   `(hl-line (,@(yester-whole-face-spec class :background current-line)))
   `(border (,@(yester-whole-face-spec class :background current-line)))
   `(linum ((,class :inherit (shadow default))))
   `(isearch-fail (,@(yester-whole-face-spec class
                       :foreground red
                       :background background
                       :weight 'bold
                       :inverse-video t)))
   `(mode-line-emphasis (,@(yester-whole-face-spec class
                             :foreground foreground :slant 'italic)))
   `(show-paren-match (,@(yester-whole-face-spec class
                           :background current-line
                           :foreground blue
                           :inverse-video t))) ;inverse is for visible-mark
   `(show-paren-mismatch (,@(yester-whole-face-spec class
                              :background current-line
                              :foreground orange
                              :inverse-video t))) ;inverse is for visible-mark
   `(which-func (,@(yester-whole-face-spec class :foreground blue)))
   `(tab-line-tab ((,class :inherit 'tab-line)))
   `(tab-line-tab-current (,@(yester-whole-face-spec class
                               :weight 'bold
                               :background background
                               :inherit 'tab-line-tab)))
   `(tab-line-tab-inactive ((,class :inherit tab-line-tab)))
   `(tab-bar-tab (,@(yester-whole-face-spec class
                      :weight 'bold
                      :background background
                      :inherit 'tab-bar)))
   `(tab-bar-tab-inactive ((,class :inherit 'tab-bar)))



   ;; Completion
   `(completions-annotations ((,class :inherit shadow)))
   `(completions-common-part
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground aqua)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground orange)))
               (t (yester-let-colors day (list :foreground aqua)))))))
   `(completions-first-difference
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground emboss)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :weight 'bold :foreground emboss)))))



   ;; Dired
   `(dired-directory (,@(yester-whole-face-spec class :foreground blue)))
   `(dired-header (,@(yester-whole-face-spec class :foreground purple)))
   `(dired-mark (,@(yester-whole-face-spec class :foreground comment)))
   `(dired-perm-write
     (,@(yester-whole-face-spec class :foreground orange :weight 'bold)))
   `(dired-set-id
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(dired-special (,@(yester-whole-face-spec class :foreground yellow)))
   `(dired-symlink (,@(yester-whole-face-spec class :foreground aqua)))
   `(dired-warning
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))



   ;; diff
   `(diff-header (,@(yester-whole-face-spec class :background current-line)))
   `(diff-file-header (,@(yester-whole-face-spec class :background selection)))
   `(diff-hunk-header (,@(yester-whole-face-spec class
                           :background current-line :foreground purple)))
   `(diff-changed ((,class)))
   `(diff-added (,@(yester-whole-face-spec class
                     :inherit 'diff-changed :background diff-green)))
   `(diff-removed (,@(yester-whole-face-spec class
                       :inherit 'diff-changed :background diff-red)))
   `(diff-refine-changed
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground diff-variant-yellow
                                        :background diff-accent-yellow)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :foreground diff-accent-yellow
                                      :background diff-variant-yellow)))))
   `(diff-refine-added
     (((,@class (background dark))
       ,@(yester-let-colors night (list :inherit 'diff-refine-changed
                                        :foreground diff-variant-green
                                        :background diff-accent-green)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :inherit 'diff-refine-changed
                                      :foreground diff-accent-green
                                      :background diff-variant-green)))))
   `(diff-refine-removed
     (((,@class (background dark))
       ,@(yester-let-colors night (list :inherit 'diff-refine-changed
                                        :foreground diff-variant-red
                                        :background diff-accent-red)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :inherit 'diff-refine-changed
                                      :foreground diff-accent-red
                                      :background diff-variant-red)))))
   `(diff-indicator-changed (,@(yester-whole-face-spec class
                                 :inherit 'diff-changed
                                 :foreground diff-accent-yellow)))
   `(diff-indicator-added (,@(yester-whole-face-spec class
                               :inherit 'diff-added
                               :foreground diff-accent-green)))
   `(diff-indicator-removed (,@(yester-whole-face-spec class
                                 :inherit 'diff-removed
                                 :foreground diff-accent-red)))



   ;; Compilation
   `(compilation-column-number
     (,@(yester-whole-face-spec class :foreground yellow)))
   `(compilation-line-number
     (,@(yester-whole-face-spec class :foreground yellow)))
   `(compilation-mode-line-exit ((,class :inherit success)))
   `(compilation-mode-line-fail ((,class :inherit error)))
   `(compilation-mode-line-run
     (,@(yester-whole-face-spec class :foreground blue)))



   ;; Outline
   `(outline-1 (,@(yester-whole-face-spec class :foreground blue)))
   `(outline-2 (,@(yester-whole-face-spec class :foreground orange)))
   `(outline-3 (,@(yester-whole-face-spec class :foreground purple)))
   `(outline-4 (,@(yester-whole-face-spec class :foreground comment)))
   `(outline-5 (,@(yester-whole-face-spec class :foreground yellow)))
   `(outline-6 (,@(yester-whole-face-spec class :foreground aqua)))
   `(outline-7 (,@(yester-whole-face-spec class :foreground green)))
   `(outline-8 (,@(yester-whole-face-spec class :foreground red)))



   ;; IDO
   `(ido-first-match
     (,@(yester-whole-face-spec class :foreground orange :weight 'bold)))
   `(ido-incomplete-regexp
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(ido-indicator
     (,@(yester-whole-face-spec class :foreground red)))
   `(ido-only-match
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(ido-subdir (,@(yester-whole-face-spec class :foreground comment)))
   `(ido-virtual (,@(yester-whole-face-spec class :foreground comment)))



   ;; El-doc
   `(eldoc-highlight-function-argument
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))



   ;; Ediff
   `(ediff-current-diff-A ((,class :inherit diff-removed)))
   `(ediff-current-diff-B ((,class :inherit diff-added)))
   `(ediff-current-diff-C
     (,@(yester-whole-face-spec class :background diff-yellow)))
   `(ediff-current-diff-Ancestor
     (,@(yester-whole-face-spec class :background diff-cyan)))
   `(ediff-fine-diff-A ((,class :inherit diff-refine-removed)))
   `(ediff-fine-diff-B ((,class :inherit diff-refine-added)))
   `(ediff-fine-diff-C ((,class :inherit diff-refine-changed)))
   `(ediff-fine-diff-Ancestor
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground diff-variant-cyan
                                        :background diff-accent-cyan)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :foreground diff-accent-cyan
                                      :background diff-variant-cyan)))))
   `(ediff-even-diff-A
     (,@(yester-whole-face-spec class :background current-line)))
   `(ediff-even-diff-B
     (,@(yester-whole-face-spec class :background current-line)))
   `(ediff-even-diff-C
     (,@(yester-whole-face-spec class :background current-line)))
   `(ediff-even-diff-Ancestor
     (,@(yester-whole-face-spec class :background current-line)))
   `(ediff-odd-diff-A
     (,@(yester-whole-face-spec class :background selection)))
   `(ediff-odd-diff-B
     (,@(yester-whole-face-spec class :background selection)))
   `(ediff-odd-diff-C
     (,@(yester-whole-face-spec class :background selection)))
   `(ediff-odd-diff-Ancestor
     (,@(yester-whole-face-spec class :background selection)))



   ;; Org-mode
   `(org-agenda-structure
     (,@(yester-whole-face-spec class :foreground purple)))
   `(org-agenda-date (,@(yester-whole-face-spec class :foreground blue)))
   `(org-agenda-done (,@(yester-whole-face-spec class :foreground purple)))
   `(org-agenda-dimmed-todo-face ((,class :inherit shadow)))
   `(org-block (,@(yester-whole-face-spec class :foreground orange)))
   `(org-code (,@(yester-whole-face-spec class :foreground yellow)))
   `(org-column (,@(yester-whole-face-spec class :background current-line)))
   `(org-column-title
     ((,class :inherit org-column :weight bold :underline t)))
   `(org-date
     (,@(yester-whole-face-spec class :foreground purple :underline t)))
   `(org-document-info (,@(yester-whole-face-spec class :foreground aqua)))
   `(org-document-info-keyword
     (,@(yester-whole-face-spec class :foreground comment)))
   `(org-document-title
     (,@(yester-whole-face-spec class :weight 'bold :foreground purple)))
   `(org-meta-line ((,class :inherit shadow)))
   `(org-done (,@(yester-whole-face-spec class :foreground green)))
   `(org-ellipsis (,@(yester-whole-face-spec class :foreground comment)))
   `(org-footnote (,@(yester-whole-face-spec class :foreground aqua)))
   `(org-formula (,@(yester-whole-face-spec class :foreground red)))
   `(org-hide (,@(yester-whole-face-spec class :foreground current-line)))
   `(org-link ((,class :inherit link)))
   `(org-scheduled (,@(yester-whole-face-spec class :foreground green)))
   `(org-scheduled-previously
     (,@(yester-whole-face-spec class :foreground orange)))
   `(org-scheduled-today
     (,@(yester-whole-face-spec class :foreground green)))
   `(org-special-keyword
     (,@(yester-whole-face-spec class :foreground purple)))
   `(org-table (,@(yester-whole-face-spec class :foreground green)))
   `(org-todo (,@(yester-whole-face-spec class :foreground red)))
   `(org-upcoming-deadline ((,class :inherit warning)))
   `(org-warning
     (,@(yester-whole-face-spec class :weight 'bold :foreground red)))
   `(org-drawer (,@(yester-whole-face-spec class :foreground aqua)))
   `(org-time-grid (,@(yester-whole-face-spec class :foreground yellow)))



   ;; Term-mode
   `(term-color-black (,@(yester-whole-face-spec class
                           :foreground background :background background)))
   `(term-color-red
     (,@(yester-whole-face-spec class :foreground red :background red)))
   `(term-color-green
     (,@(yester-whole-face-spec class :foreground green :background green)))
   `(term-color-yellow
     (,@(yester-whole-face-spec class :foreground yellow :background yellow)))
   `(term-color-blue
     (,@(yester-whole-face-spec class :foreground blue :background blue)))
   `(term-color-magenta
     (,@(yester-whole-face-spec class :foreground purple :background purple)))
   `(term-color-cyan
     (,@(yester-whole-face-spec class :foreground aqua :background aqua)))
   `(term-color-white (,@(yester-whole-face-spec class
                           :foreground foreground :background foreground)))



   ;; Flyspell
   `(flyspell-duplicate (,@(yester-whole-face-spec class
                             :underline `(:style wave :color ,orange))))
   `(flyspell-incorrect (,@(yester-whole-face-spec class
                             :underline `(:style wave :color ,red))))



   ;; Eww
   `(eww-invalid-certificate ((,class :inherit error)))
   `(eww-valid-certificate ((,class :inherit success)))



   ;; Smerge
   `(smerge-base (,@(yester-whole-face-spec class :background diff-yellow)))
   `(smerge-lower ((,class :inherit diff-added)))
   `(smerge-markers
     (,@(yester-whole-face-spec class :background selection)))
   `(smerge-refined-added ((,class :inherit diff-refine-added)))
   `(smerge-refined-removed ((,class :inherit diff-refine-removed)))
   `(smerge-upper ((,class :inherit diff-removed)))



   ;; Customize
   `(custom-button (,@(yester-whole-face-spec class
                        :foreground foreground
                        :background current-line
                        :box selection)))
   `(custom-button-pressed (,@(yester-whole-face-spec class
                                :foreground foreground
                                :background current-line
                                :box selection)))
   `(custom-variable-tag
     (,@(yester-whole-face-spec class :foreground aqua :weight 'bold)))
   `(custom-group-tag (,@(yester-whole-face-spec class
                           :height 1.2
                           :weight 'bold
                           :foreground aqua
                           :inherit 'variable-pitch)))
   `(custom-state (,@(yester-whole-face-spec class :foreground green)))



   ;; Hi-lock
   `(hi-blue
     (,@(yester-whole-face-spec class :foreground blue :inverse-video t)))
   `(hi-blue-b
     (,@(yester-whole-face-spec class :foreground blue :weight 'bold)))
   `(hi-green
     (,@(yester-whole-face-spec class :foreground green :inverse-video t)))
   `(hi-green-b
     (,@(yester-whole-face-spec class :foreground green :weight 'bold)))
   `(hi-pink
     (,@(yester-whole-face-spec class :foreground red :inverse-video t)))
   `(hi-red-b
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))
   `(hi-yellow
     (,@(yester-whole-face-spec class :foreground yellow :inverse-video t)))
   `(hi-aquamarine
     (,@(yester-whole-face-spec class :foreground aqua :inverse-video t)))
   `(hi-salmon
     (,@(yester-whole-face-spec class :foreground orange :inverse-video t)))



   ;; Popup
   `(popup-face ((,class :inherit default)))
   `(popup-isearch-match ((,class :inherit (lazy-highlight default))))
   `(popup-menu-mouse-face ((,class :inherit popup-menu-selection-face)))
   `(popup-menu-selection-face
     (,@(yester-whole-face-spec class :background selection :inherit 'default)))
   `(popup-scroll-bar-background-face
     (,@(yester-whole-face-spec class :background current-line)))
   `(popup-scroll-bar-foreground-face
     (,@(yester-whole-face-spec class :background selection)))
   `(popup-summary-face ((,class :inherit (shadow popup-face))))
   `(popup-tip-face
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground foreground
                                        :background selection
                                        :inherit 'popup-face)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground foreground
                                             :background selection
                                             :inherit 'popup-face)))
               (t (yester-let-colors day (list :foreground foreground
                                               :background current-line
                                               :inherit 'popup-face)))))))



   ;; Auto-complete
   `(ac-completion-face ((,class :inherit shadow :underline t)))
   `(ac-gtags-candidate-face
     ((t :inherit (font-lock-function-name-face ac-candidate-face))))
   `(ac-gtags-selection-face
     ((t :inherit (font-lock-function-name-face ac-selection-face))))
   `(ac-yasnippet-candidate-face
     ((t :inherit (font-lock-warning-face ac-candidate-face))))
   `(ac-yasnippet-selection-face
     ((t :inherit (font-lock-warning-face ac-selection-face))))



   ;; Visible-mark
   `(visible-mark-face ((,class :inverse-video t)))



   ;; Ace-jump-mode
   `(ace-jump-face-foreground
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground emboss)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :weight 'bold :foreground emboss)))))
   `(ace-jump-face-background ((,class :inherit shadow)))



   ;; Volatile-highlights
   `(vhl/default-face ((,class :inherit region)))



   ;; Anzu
   `(anzu-match-1 ((,class :inherit query-replace)))
   `(anzu-match-2 ((,class :inherit query-replace)))
   `(anzu-match-3 ((,class :inherit query-replace)))
   `(anzu-mode-line (,@(yester-whole-face-spec class :foreground aqua)))
   `(anzu-mode-line-no-match
     (,@(yester-whole-face-spec class :foreground aqua)))
   `(anzu-replace-highlight ((,class :inherit lazy-highlight)))
   `(anzu-replace-to
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground emboss)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :weight 'bold :foreground emboss)))))



   ;; Helm
   `(helm-action ((,class)))
   `(helm-M-x-key
     (,@(yester-whole-face-spec class :foreground  orange :underline t)))
   `(helm-source-header (,@(yester-whole-face-spec class
                               :weight 'bold
                               :height 1.4
                               :background current-line
                               :inherit 'variable-pitch)))
   `(helm-candidate-number
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground yellow)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground red)))
               (t (yester-let-colors day
                    (list :foreground blue :weight 'bold)))))))
   `(helm-header-line-left-margin
     (,@(yester-whole-face-spec class :foreground yellow :inverse-video t)))
   `(helm-selection (,@(yester-whole-face-spec class :background selection)))
   `(helm-selection-line ((,class :weight bold)))
   `(helm-separator ((,class :inherit error)))
   `(helm-visible-mark
     (((,@class (background dark))
       ,@(yester-let-colors night (list :underline yellow)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :underline red)))
               (t (yester-let-colors day (list :underline blue)))))))
   `(helm-buffer-directory ((,class :inherit dired-directory)))
   `(helm-buffer-file ((,class)))
   `(helm-buffer-not-saved ((,class :inherit warning)))
   `(helm-buffer-saved-out ((,class :inherit error)))
   `(helm-buffer-modified (,@(yester-whole-face-spec class :foreground yellow)))
   `(helm-buffer-size ((,class :slant italic :inherit shadow)))
   `(helm-buffer-process (,@(yester-whole-face-spec class :foreground aqua)))
   `(helm-ff-denied ((,class :slant italic :inherit dired-warning)))
   `(helm-ff-directory ((,class :inherit dired-directory)))
   `(helm-ff-dirs ((,class :inherit dired-directory :inverse-video t)))
   `(helm-ff-dotted-directory
     ((,class :slant italic :inherit helm-ff-directory)))
   `(helm-ff-dotted-symlink-directory
     ((,class :slant italic :inherit helm-ff-symlink)))
   `(helm-ff-executable (,@(yester-whole-face-spec class :foreground green)))
   `(helm-ff-file ((,class)))
   `(helm-ff-truename ((,class :inherit shadow)))
   `(helm-ff-file-extension ((,class :inherit helm-ff-file)))
   `(helm-ff-invalid-symlink ((,class :inherit dired-warning)))
   `(helm-ff-pipe ((,class :inherit dired-special)))
   `(helm-ff-prefix
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground yellow :weight 'bold)))
      ((,@class (background light))
       ,@(cond ((eq (cdr (assq 'day yester-scene)) 'morning)
                (yester-let-colors day (list :foreground red :weight 'bold)))
               (t (yester-let-colors day
                    (list :foreground blue :weight 'bold)))))))
   `(helm-ff-socket ((,class :slant italic :inherit dired-special)))
   `(helm-ff-suid ((,class :inherit dired-set-id)))
   `(helm-ff-symlink ((,class :inherit dired-symlink)))
   `(helm-grep-file ((,class :inherit compilation-info)))
   `(helm-grep-finish ((,class :inherit compilation-mode-line-exit)))
   `(helm-grep-lineno ((,class :inherit compilation-line-number)))
   `(helm-grep-match ((,class :inherit helm-match :inverse-video t)))
   `(helm-locate-finish ((,class :inherit success)))
   `(helm-match
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground emboss)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :weight 'bold :foreground emboss)))))
   `(helm-moccur-buffer (,@(yester-whole-face-spec class :foreground aqua)))
   `(helm-mode-prefix
     (,@(yester-whole-face-spec class :foreground yellow :inverse-video t)))



   ;; Helm-swoop
   `(helm-swoop-line-number-face ((,class :inherit shadow)))
   `(helm-swoop-target-line-block-face
     (,@(yester-whole-face-spec class :background current-line)))
   `(helm-swoop-target-line-face
     (,@(yester-whole-face-spec class :background selection)))
   `(helm-swoop-target-word-face ((,class :inherit helm-match)))



   ;; Rainbow-delimiters
   `(rainbow-delimiters-base-error-face
     (,@(yester-whole-face-spec class
          :foreground red :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-1-face
     (,@(yester-whole-face-spec class :foreground purple)))
   `(rainbow-delimiters-depth-2-face
     (,@(yester-whole-face-spec class :foreground blue)))
   `(rainbow-delimiters-depth-3-face
     (,@(yester-whole-face-spec class :foreground aqua)))
   `(rainbow-delimiters-depth-4-face
     (,@(yester-whole-face-spec class :foreground green)))
   `(rainbow-delimiters-depth-5-face
     (,@(yester-whole-face-spec class :foreground yellow)))
   `(rainbow-delimiters-depth-6-face
     (,@(yester-whole-face-spec class :foreground orange)))
   `(rainbow-delimiters-depth-7-face
     (,@(yester-whole-face-spec class :foreground red)))
   `(rainbow-delimiters-depth-8-face
     (,@(yester-whole-face-spec class :foreground comment)))
   `(rainbow-delimiters-depth-9-face
     (,@(yester-whole-face-spec class :foreground foreground)))
   `(rainbow-delimiters-unmatched-face
     (,@(yester-whole-face-spec class :foreground red :weight 'bold)))



   ;; Git-gutter-fringe
   `(git-gutter-fr:modified
     (,@(yester-whole-face-spec class :foreground diff-accent-yellow)))
   `(git-gutter-fr:added
     (,@(yester-whole-face-spec class :foreground diff-accent-green)))
   `(git-gutter-fr:deleted
     (,@(yester-whole-face-spec class :foreground diff-accent-red)))



   ;; Magit
   `(magit-branch-local (,@(yester-whole-face-spec class :foreground aqua)))
   `(magit-branch-remote
     (,@(yester-whole-face-spec class :foreground blue)))
   `(magit-hash (,@(yester-whole-face-spec class :foreground comment)))
   `(magit-header-line (,@(yester-whole-face-spec class :foreground purple)))
   `(magit-log-author (,@(yester-whole-face-spec class :foreground orange)))
   `(magit-log-graph (,@(yester-whole-face-spec class :foreground comment)))
   `(magit-section-heading
     (,@(yester-whole-face-spec class :foreground yellow :weight 'bold)))
   `(magit-section-heading-selection
     (,@(yester-whole-face-spec class :foreground purple)))
   `(magit-section-highlight (,@(yester-whole-face-spec class
                                  :background current-line)))
   `(magit-process-ng (,@(yester-whole-face-spec class
                           :foreground red :inherit 'magit-section-heading)))
   `(magit-process-ok (,@(yester-whole-face-spec class
                           :foreground green :inherit 'magit-section-heading)))
   `(magit-diff-context ((,class :inherit diff-context)))
   `(magit-diff-context-highlight (,@(yester-whole-face-spec class
                                       :inherit 'magit-diff-context
                                       :background current-line)))
   `(magit-diff-file-heading-selection
     (,@(yester-whole-face-spec class
          :inherit 'magit-diff-file-heading-highlight
          :foreground purple)))
   `(magit-diff-hunk-heading ((,class :inherit 'diff-hunk-header)))
   `(magit-diff-hunk-heading-highlight (,@(yester-whole-face-spec class
                                            :inherit 'magit-diff-hunk-heading
                                            :background current-line)))
   `(magit-diff-hunk-heading-selection
     (,@(yester-whole-face-spec class
          :inherit 'magit-diff-hunk-heading-highlight
          :foreground purple)))
   `(magit-diff-lines-heading
     (,@(yester-whole-face-spec class
          :inverse-video t
          :inherit 'magit-diff-hunk-heading-highlight)))
   `(magit-diff-lines-boundary (,@(yester-whole-face-spec class
                                    :foreground background
                                    :background selection)))
   `(magit-diff-added
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground diff-variant-green
                                        :background diff-green)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :foreground diff-accent-green
                                      :background diff-green)))))
   `(magit-diff-added-highlight ((,class :inherit magit-diff-added)))
   `(magit-diff-removed
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground diff-variant-red
                                        :background diff-red)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :foreground diff-accent-red
                                      :background diff-red)))))
   `(magit-diff-removed-highlight ((,class :inherit magit-diff-removed)))
   `(magit-diff-base
     (((,@class (background dark))
       ,@(yester-let-colors night (list :foreground diff-variant-yellow
                                        :background diff-yellow)))
      ((,@class (background light))
       ,@(yester-let-colors day (list :foreground diff-accent-yellow
                                      :background diff-yellow)))))
   `(magit-diff-base-highlight ((,class :inherit magit-diff-base)))
   `(magit-diffstat-added
     (,@(yester-whole-face-spec class :foreground diff-accent-green)))
   `(magit-diffstat-removed
     (,@(yester-whole-face-spec class :foreground diff-accent-red)))



   ;; Flycheck
   `(flycheck-info (,@(yester-whole-face-spec class
                        :underline `(:style wave :color ,green))))
   `(flycheck-warning (,@(yester-whole-face-spec class
                           :underline `(:style wave :color ,orange))))
   `(flycheck-error (,@(yester-whole-face-spec class
                         :underline `(:style wave :color ,red))))



   ;; Evil
   `(evil-ex-info
     (,@(yester-whole-face-spec class :foreground red :slant 'italic)))
   `(evil-ex-substitute-matches
     (,@(yester-whole-face-spec class :foreground red :underline t)))



   ;; Flex-isearch
   `(flex-isearch-message-prefix
     (,@(yester-whole-face-spec class :foreground aqua)))



   ;; Wgrep
   `(wgrep-face
     (,@(yester-whole-face-spec class :foreground aqua :inverse-video t)))
   `(wgrep-done-face (,@(yester-whole-face-spec class :foreground blue)))



   ;; Color-moccur
   `(moccur-face ((,class :inherit match)))



   ;; Skk
   `(skk-henkan-face-default
     (,@(yester-whole-face-spec class :foreground background :background aqua)))
   `(skk-prefix-hiragana-face
     (,@(yester-whole-face-spec class :foreground blue)))
   `(skk-prefix-jisx0201-face
     (,@(yester-whole-face-spec class :foreground comment)))
   `(skk-prefix-katakana-face
     (,@(yester-whole-face-spec class :foreground aqua)))))



(custom-theme-set-variables
 'yester

 ;; Popup
 `(popup-isearch-cursor-color
   ,(yester-whole-symbol-exp yellow))



 ;; Auto-complete
 `(ac-fuzzy-cursor-color
   ,(yester-whole-symbol-exp orange))



 ;; Smartrep
 `(smartrep-mode-line-active-bg ,(yester-whole-symbol-exp selection))



 ;; Evil
 `(evil-emacs-state-cursor ,(yester-whole-symbol-exp red))
 `(evil-normal-state-cursor ,(yester-whole-symbol-exp green))
 `(evil-insert-state-cursor ,(yester-whole-symbol-exp `(bar ,green)))
 `(evil-replace-state-cursor ,(yester-whole-symbol-exp `(hbar ,green)))
 `(evil-operator-state-cursor ,(yester-whole-symbol-exp green))
 `(evil-visual-state-cursor ,(yester-whole-symbol-exp green))
 `(evil-motion-state-cursor ,(yester-whole-symbol-exp purple))



 ;; Skk
 `(skk-use-color-cursor t)
 `(skk-cursor-abbrev-color ,(yester-whole-symbol-exp comment))
 `(skk-cursor-jisx0208-latin-color ,(yester-whole-symbol-exp comment))
 `(skk-cursor-katakana-color ,(yester-whole-symbol-exp aqua))
 `(skk-cursor-hiragana-color ,(yester-whole-symbol-exp blue))
 `(skk-cursor-jisx0201-color ,(yester-whole-symbol-exp comment))
 `(skk-cursor-latin-color ,(yester-whole-symbol-exp red)))

(provide-theme 'yester)

;;; yester-theme.el ends here
