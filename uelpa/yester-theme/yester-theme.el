;;; yester-theme.el --- Custom theme for faces derived from tomorrow theme

;; Copyright (C) 2020, 2021 0x60DF

;;; Commentary:

;; Theme settings

;; Color scheme is designed by Chris Kempson:
;; https://github.com/chriskempson/tomorrow-theme

;;; Code:

(require 'yester)

(deftheme yester "Custom theme for faces derived from tomorrow theme.")

(let ((class '((class color) (min-colors 89))))
  (yester-theme-set-faces
   'yester

   ;; Basic faces
   `(default
      (yester-face-spec ',class :foreground foreground :background background))
   `(bold '((,class :weight bold)))
   `(italic '((,class :slant italic)))
   `(bold-italic '((,class :slant italic :weight bold)))
   `(underline '((,class :underline t)))

   `(shadow (yester-face-spec ',class :foreground comment))
   `(link (yester-face-spec ',class :foreground blue))
   `(link-visited (yester-face-spec ',class :foreground purple))
   `(highlight (yester-face-spec ',class
                 :foreground green :background background :inverse-video t))
   `(match (yester-face-spec ',class
             :foreground blue :background background :inverse-video t))
   `(isearch (yester-face-spec ',class
               :foreground yellow :background background :inverse-video t))
   `(lazy-highlight (yester-face-spec ',class
                      :foreground aqua :background background :inverse-video t))
   `(error (yester-face-spec ',class :foreground red))
   `(warning (yester-face-spec ',class :foreground orange))
   `(success (yester-face-spec ',class :foreground green))



   ;; Font Lock
   `(font-lock-builtin-face (yester-face-spec ',class :foreground aqua))
   `(font-lock-comment-delimiter-face
     (yester-face-spec ',class :foreground comment :slant 'italic))
   `(font-lock-comment-face
     (yester-face-spec ',class :foreground comment :slant 'italic))
   `(font-lock-constant-face (yester-face-spec ',class :foreground aqua))
   `(font-lock-doc-face (yester-face-spec ',class :foreground comment))
   `(font-lock-function-name-face (yester-face-spec ',class :foreground blue))
   `(font-lock-keyword-face (yester-face-spec ',class :foreground purple))
   `(font-lock-negation-char-face (yester-face-spec ',class :foreground green))
   `(font-lock-preprocessor-face (yester-face-spec ',class :foreground purple))
   `(font-lock-regexp-grouping-backslash
     (yester-face-spec ',class :foreground yellow))
   `(font-lock-regexp-grouping-construct
     (yester-face-spec ',class :foreground purple))
   `(font-lock-string-face (yester-face-spec ',class :foreground green))
   `(font-lock-type-face (yester-face-spec ',class :foreground yellow))
   `(font-lock-variable-name-face (yester-face-spec ',class :foreground orange))
   `(font-lock-warning-face
     (yester-face-spec ',class :weight 'bold :foreground red))



   ;; Standard Faces
   `(region (yester-face-spec ',class :background selection))
   `(secondary-selection (yester-face-spec ',class :background current-line))
   `(trailing-whitespace
     (yester-face-spec ',class :background red :foreground yellow))
   `(escape-glyph (yester-face-spec ',class :foreground red))
   `(homoglyph (yester-face-spec ',class :foreground red :weight 'bold))
   `(nobreak-space (yester-face-spec ',class :box red))
   `(nobreak-hyphen (yester-face-spec ',class :foreground red :weight 'bold))
   `(mode-line
     (yester-face-spec ',class :background selection :foreground foreground))
   `(mode-line-inactive
     (yester-face-spec ',class :background current-line :foreground foreground))
   `(mode-line-highlight
     (yester-face-spec ',class :foreground purple :weight 'bold))
   `(mode-line-buffer-id
     (yester-face-spec ',class :foreground purple :weight 'bold))
   `(header-line
     (yester-face-spec ',class :foreground purple :inherit 'mode-line))
   `(header-line-highlight
     (yester-face-spec ',class :foreground purple :weight 'bold))
   `(tab-line (yester-face-spec ',class
                :foreground foreground
                :background current-line
                :inherit 'variable-pitch))
   `(minibuffer-prompt (yester-face-spec ',class :foreground blue))
   `(fringe (yester-face-spec ',class :background current-line))
   `(cursor (yester-face-spec ',class :background red))
   `(tooltip
     (yester-face-spec ',class :background selection :foreground foreground))
   `(tool-bar
     (yester-face-spec ',class :foreground foreground :background current-line))
   `(tab-bar (yester-face-spec ',class
               :foreground foreground
               :background current-line
               :inherit 'variable-pitch))



   ;; Complementary Faces for Basic and Standard Faces
   `(hl-line (yester-face-spec ',class :background current-line))
   `(border (yester-face-spec ',class :background background))
   `(internal-border (yester-face-spec ',class :background background))
   `(vertical-border (yester-face-spec ',class :foreground selection))
   `(linum '((,class :inherit (shadow default))))
   `(isearch-fail (yester-face-spec ',class
                    :foreground red
                    :background background
                    :weight 'bold
                    :inverse-video t))
   `(mode-line-emphasis
     (yester-face-spec ',class :foreground foreground :slant 'italic))
   `(show-paren-match (yester-face-spec ',class
                        :background background
                        :foreground blue
                        :inverse-video t)) ;inverse is for visible-mark
   `(show-paren-mismatch (yester-face-spec ',class
                           :background background
                           :foreground orange
                           :inverse-video t)) ;inverse is for visible-mark
   `(show-paren-match-expression
     (yester-face-spec ',class :background current-line))
   `(which-func (yester-face-spec ',class :foreground blue))
   `(tab-line-tab '((,class :inherit 'tab-line)))
   `(tab-line-tab-current (yester-face-spec ',class
                            :weight 'bold
                            :background background
                            :inherit 'tab-line-tab))
   `(tab-line-tab-inactive '((,class :inherit tab-line-tab)))
   `(tab-bar-tab (yester-face-spec ',class
                   :weight 'bold
                   :background background
                   :inherit 'tab-bar))
   `(tab-bar-tab-inactive '((,class :inherit 'tab-bar)))
   `(line-number-major-tick (yester-face-spec ',class
                              :weight 'bold
                              :background selection
                              :inherit 'line-number))
   `(line-number-minor-tick
     (yester-face-spec ',class :background current-line :inherit 'line-number))
   `(window-divider (yester-face-spec ',class :foreground selection))
   `(window-divider-first-pixel
     (yester-face-spec ',class :foreground selection))
   `(window-divider-last-pixel (yester-face-spec ',class :foreground selection))



   ;; VC
   `(vc-conflict-state
     (yester-face-spec ',class :foreground red :inherit 'vc-state-base))
   `(vc-edited-state
     (yester-face-spec ',class :foreground yellow :inherit 'vc-state-base))
   `(vc-locally-added-state
     (yester-face-spec ',class :foreground yellow :inherit 'vc-state-base))
   `(vc-locked-state
     (yester-face-spec ',class :foreground orange :inherit 'vc-state-base))
   `(vc-missing-state
     (yester-face-spec ',class :foreground orange :inherit 'vc-state-base))
   `(vc-needs-update-state
     (yester-face-spec ',class :foreground aqua :inherit 'vc-state-base))
   `(vc-removed-state
     (yester-face-spec ',class :foreground orange :inherit 'vc-state-base))
   `(vc-up-to-date-state
     (yester-face-spec ',class :foreground green :inherit 'vc-state-base))



   ;; Completion
   `(completions-annotations '((,class :inherit shadow)))
   `(completions-common-part (yester-face-spec ',class
                               (night :foreground aqua)
                               (day (nil :foreground aqua)
                                    (morning :foreground orange))))
   `(completions-first-difference (yester-face-spec ',class
                                    (night :foreground emboss)
                                    (day :weight 'bold :foreground emboss)))



   ;; Dired
   `(dired-directory (yester-face-spec ',class :foreground blue))
   `(dired-header (yester-face-spec ',class :foreground purple))
   `(dired-mark (yester-face-spec ',class :foreground comment))
   `(dired-perm-write
     (yester-face-spec ',class :foreground orange :weight 'bold))
   `(dired-set-id (yester-face-spec ',class :foreground red :weight 'bold))
   `(dired-special (yester-face-spec ',class :foreground yellow))
   `(dired-symlink (yester-face-spec ',class :foreground aqua))
   `(dired-warning (yester-face-spec ',class :foreground red :weight 'bold))



   ;; Diff
   `(diff-header
     (yester-face-spec ',class :foreground comment :background block))
   `(diff-file-header
     (yester-face-spec ',class :weight 'bold :foreground comment))
   `(diff-hunk-header
     (yester-face-spec ',class :weight 'bold :foreground purple))
   `(diff-changed '((,class)))
   `(diff-added
     (yester-face-spec ',class :inherit 'diff-changed :background diff-green))
   `(diff-removed
     (yester-face-spec ',class :inherit 'diff-changed :background diff-red))
   `(diff-refine-changed (yester-face-spec ',class
                           (night :foreground diff-variant-yellow
                                  :background diff-accent-yellow)
                           (day :foreground diff-accent-yellow
                                :background diff-variant-yellow)))
   `(diff-refine-added (yester-face-spec ',class
                         (night :inherit 'diff-refine-changed
                                :foreground diff-variant-green
                                :background diff-accent-green)
                         (day :inherit 'diff-refine-changed
                              :foreground diff-accent-green
                              :background diff-variant-green)))
   `(diff-refine-removed (yester-face-spec ',class
                           (night :inherit 'diff-refine-changed
                                  :foreground diff-variant-red
                                  :background diff-accent-red)
                           (day :inherit 'diff-refine-changed
                                :foreground diff-accent-red
                                :background diff-variant-red)))
   `(diff-indicator-changed (yester-face-spec ',class
                              :inherit 'diff-changed
                              :foreground diff-accent-yellow))
   `(diff-indicator-added (yester-face-spec ',class
                            :inherit 'diff-added
                            :foreground diff-accent-green))
   `(diff-indicator-removed (yester-face-spec ',class
                              :inherit 'diff-removed
                              :foreground diff-accent-red))



   ;; Compilation
   `(compilation-column-number (yester-face-spec ',class :foreground yellow))
   `(compilation-line-number (yester-face-spec ',class :foreground yellow))
   `(compilation-mode-line-exit '((,class :inherit success)))
   `(compilation-mode-line-fail '((,class :inherit error)))
   `(compilation-mode-line-run (yester-face-spec ',class :foreground blue))



   ;; Outline
   `(outline-1 (yester-face-spec ',class :foreground blue))
   `(outline-2 (yester-face-spec ',class :foreground orange))
   `(outline-3 (yester-face-spec ',class :foreground purple))
   `(outline-4 (yester-face-spec ',class :foreground comment))
   `(outline-5 (yester-face-spec ',class :foreground yellow))
   `(outline-6 (yester-face-spec ',class :foreground aqua))
   `(outline-7 (yester-face-spec ',class :foreground green))
   `(outline-8 (yester-face-spec ',class :foreground red))



   ;; IDO
   `(ido-first-match
     (yester-face-spec ',class :foreground orange :weight 'bold))
   `(ido-incomplete-regexp
     (yester-face-spec ',class :foreground red :weight 'bold))
   `(ido-indicator (yester-face-spec ',class :foreground red))
   `(ido-only-match (yester-face-spec ',class :foreground red :weight 'bold))
   `(ido-subdir (yester-face-spec ',class :foreground comment))
   `(ido-virtual (yester-face-spec ',class :foreground comment))



   ;; El-doc
   `(eldoc-highlight-function-argument
     (yester-face-spec ',class :foreground green :weight 'bold))



   ;; Xref
   `(xref-file-header (yester-face-spec ',class :foreground comment :extend t))



   ;; Ediff
   `(ediff-current-diff-A '((,class :inherit diff-removed)))
   `(ediff-current-diff-B '((,class :inherit diff-added)))
   `(ediff-current-diff-C (yester-face-spec ',class :background diff-yellow))
   `(ediff-current-diff-Ancestor
     (yester-face-spec ',class :background diff-cyan))
   `(ediff-fine-diff-A '((,class :inherit diff-refine-removed)))
   `(ediff-fine-diff-B '((,class :inherit diff-refine-added)))
   `(ediff-fine-diff-C '((,class :inherit diff-refine-changed)))
   `(ediff-fine-diff-Ancestor (yester-face-spec ',class
                                (night :foreground diff-variant-cyan
                                       :background diff-accent-cyan)
                                (day :foreground diff-accent-cyan
                                     :background diff-variant-cyan)))
   `(ediff-even-diff-A (yester-face-spec ',class :background current-line))
   `(ediff-even-diff-B (yester-face-spec ',class :background current-line))
   `(ediff-even-diff-C (yester-face-spec ',class :background current-line))
   `(ediff-even-diff-Ancestor
     (yester-face-spec ',class :background current-line))
   `(ediff-odd-diff-A (yester-face-spec ',class :background selection))
   `(ediff-odd-diff-B (yester-face-spec ',class :background selection))
   `(ediff-odd-diff-C (yester-face-spec ',class :background selection))
   `(ediff-odd-diff-Ancestor (yester-face-spec ',class :background selection))



   ;; Org-mode
   `(org-agenda-structure (yester-face-spec ',class :foreground purple))
   `(org-agenda-date (yester-face-spec ',class :foreground blue))
   `(org-agenda-done (yester-face-spec ',class :foreground green))
   `(org-agenda-dimmed-todo-face '((,class :inherit shadow)))
   `(org-block (yester-face-spec ',class :background block :extend t))
   `(org-code (yester-face-spec ',class :foreground yellow))
   `(org-verbatim (yester-face-spec ',class :foreground yellow))
   `(org-column (yester-face-spec ',class :background current-line))
   `(org-column-title '((,class :inherit org-column :weight bold :underline t)))
   `(org-date (yester-face-spec ',class :foreground purple :underline t))
   `(org-document-info (yester-face-spec ',class :foreground aqua))
   `(org-document-info-keyword '((,class :inherit shadow)))
   `(org-document-title
     (yester-face-spec ',class :weight 'bold :foreground purple))
   `(org-meta-line '((,class :inherit shadow)))
   `(org-done (yester-face-spec ',class :foreground green))
   `(org-ellipsis (yester-face-spec ',class :foreground comment))
   `(org-footnote (yester-face-spec ',class :foreground aqua))
   `(org-formula
     (yester-face-spec ',class :foreground green :inherit 'org-table))
   `(org-headline-done '((,class :inherit org-done :slant italic)))
   `(org-headline-todo '((,class :inherit org-todo :slant italic)))
   `(org-hide (yester-face-spec ',class :foreground background))
   `(org-link '((,class :inherit link)))
   `(org-scheduled (yester-face-spec ',class :foreground foreground))
   `(org-scheduled-previously (yester-face-spec ',class :foreground orange))
   `(org-scheduled-today (yester-face-spec ',class :foreground yellow))
   `(org-special-keyword (yester-face-spec ',class :foreground purple))
   `(org-table (yester-face-spec ',class :background block))
   `(org-todo (yester-face-spec ',class :foreground red))
   `(org-upcoming-deadline '((,class :inherit warning)))
   `(org-warning (yester-face-spec ',class :weight 'bold :foreground red))
   `(org-drawer (yester-face-spec ',class :foreground aqua))
   `(org-time-grid (yester-face-spec ',class :foreground yellow))
   `(org-date-selected (yester-face-spec ',class :inverse-video t))
   `(org-clock-overlay (yester-face-spec ',class :background selection))
   `(org-mode-line-clock (yester-face-spec ',class :foreground comment))
   `(org-mode-line-clock-overrun (yester-face-spec ',class :foreground red))
   `(org-latex-and-related (yester-face-spec ',class :foreground orange))
   `(org-macro (yester-face-spec ',class :foreground yellow))
   `(org-sexp-date (yester-face-spec ',class :foreground purple))



   ;; Term-mode
   `(term-color-black
     (yester-face-spec ',class :foreground background :background background))
   `(term-color-red
     (yester-face-spec ',class :foreground red :background red))
   `(term-color-green
     (yester-face-spec ',class :foreground green :background green))
   `(term-color-yellow
     (yester-face-spec ',class :foreground yellow :background yellow))
   `(term-color-blue
     (yester-face-spec ',class :foreground blue :background blue))
   `(term-color-magenta
     (yester-face-spec ',class :foreground purple :background purple))
   `(term-color-cyan
     (yester-face-spec ',class :foreground aqua :background aqua))
   `(term-color-white
     (yester-face-spec ',class :foreground foreground :background foreground))



   ;; Flyspell
   `(flyspell-duplicate
     (yester-face-spec ',class :underline `(:style wave :color ,orange)))
   `(flyspell-incorrect
     (yester-face-spec ',class :underline `(:style wave :color ,red)))



   ;; Sh-script
   `(sh-escaped-newline '((,class :inherit 'font-lock-builtin-face)))
   `(sh-heredoc '((,class :inherit 'font-lock-string-face)))
   `(sh-quoted-exec (yester-face-spec ',class :foreground green :weight 'bold))



   ;; Eshell
   `(eshell-prompt (yester-face-spec ',class :foreground purple))
   `(eshell-ls-archive '((,class :slant italic)))
   `(eshell-ls-backup '((,class :inherit shadow)))
   `(eshell-ls-clutter
     (yester-face-spec ',class :slant 'italic :foreground orange))
   `(eshell-ls-directory '((,class :inherit dired-directory)))
   `(eshell-ls-executable (yester-face-spec ',class :foreground green))
   `(eshell-ls-missing (yester-face-spec ',class :foreground red))
   `(eshell-ls-product (yester-face-spec ',class :foreground orange))
   `(eshell-ls-readonly '((,class :weight bold)))
   `(eshell-ls-special '((,class :inherit dired-special)))
   `(eshell-ls-symlink '((,class :inherit dired-symlink)))
   `(eshell-ls-unreadable '((,class :weight bold :inherit shadow)))



   ;; Eww
   `(eww-invalid-certificate '((,class :inherit error)))
   `(eww-valid-certificate '((,class :inherit success)))
   `(eww-form-checkbox (yester-face-spec ',class
                         :foreground foreground
                         :background current-line
                         :box selection))
   `(eww-form-file (yester-face-spec ',class
                     :foreground foreground
                     :background current-line
                     :box selection))
   `(eww-form-select (yester-face-spec ',class
                       :foreground foreground
                       :background current-line
                       :box selection))
   `(eww-form-submit (yester-face-spec ',class
                       :foreground foreground
                       :background current-line
                       :box selection))
   `(eww-form-text (yester-face-spec ',class
                     :foreground selection
                     :background foreground
                     :inverse-video t
                     :distant-foreground comment))
   `(eww-form-textarea (yester-face-spec ',class
                         :foreground selection
                         :background foreground
                         :inverse-video t
                         :distant-foreground comment))



   ;; Calendar
   `(calendar-today (yester-face-spec ',class :weight 'bold :foreground yellow))
   `(calendar-month-header (yester-face-spec ',class :foreground blue))
   `(calendar-weekday-header (yester-face-spec ',class :foreground aqua))
   `(calendar-weekend-header (yester-face-spec ',class :foreground comment))
   `(holiday (yester-face-spec ',class :slant 'italic :foreground purple))



   ;; Widget
   `(widget-button (yester-face-spec ',class
                     :foreground foreground
                     :background current-line
                     :box selection))
   `(widget-button-pressed (yester-face-spec ',class
                             :weight 'bold
                             :foreground foreground
                             :background selection
                             :box selection))
   `(widget-documentation (yester-face-spec ',class :foreground comment))
   `(widget-field (yester-face-spec ',class :background selection))
   `(widget-single-line-field (yester-face-spec ',class :background selection))



   ;; Smerge
   `(smerge-base (yester-face-spec ',class :background diff-yellow))
   `(smerge-lower '((,class :inherit diff-added)))
   `(smerge-markers (yester-face-spec ',class :background selection))
   `(smerge-refined-added '((,class :inherit diff-refine-added)))
   `(smerge-refined-removed '((,class :inherit diff-refine-removed)))
   `(smerge-upper '((,class :inherit diff-removed)))



   ;; Customize
   `(custom-button '((,class :inherit widget-button)))
   `(custom-button-mouse '((,class :inherit custom-button)))
   `(custom-button-pressed '((,class :inherit widget-button-pressed)))
   `(custom-button-pressed-unraised
     (yester-face-spec ',class
       :foreground purple :inherit 'custom-button-unraised))
   `(custom-variable-tag
     (yester-face-spec ',class :foreground blue :weight 'bold))
   `(custom-variable-obsolete
     (yester-face-spec ',class :foreground comment :weight 'bold))
   `(custom-variable-button
     '((,class :weight bold :underline t :inherit custom-variable-tag)))
   `(custom-comment-tag
     (yester-face-spec ',class :foreground comment :slant 'italic))
   `(custom-comment '((,class :inherit (custom-comment-tag widget-field))))
   `(custom-group-tag (yester-face-spec ',class
                        :height 1.2
                        :weight 'bold
                        :inherit 'variable-pitch))
   `(custom-group-tag-1 '((,class :height 1.2 :inherit custom-group-tag)))
   `(custom-state (yester-face-spec ',class :foreground comment))
   `(custom-documentation '((,class)))
   `(custom-invalid (yester-face-spec ',class :foreground red))
   `(custom-modified (yester-face-spec ',class :foreground orange))
   `(custom-set (yester-face-spec ',class :foreground yellow))
   `(custom-changed (yester-face-spec ',class :foreground aqua))
   `(custom-saved (yester-face-spec ',class :foreground green))
   `(custom-rogue (yester-face-spec ',class :foreground red :inverse-video t))
   `(custom-themed (yester-face-spec ',class :foreground purple))



   ;; Info
   `(info-node '((,class :weight bold)))
   `(info-menu-star (yester-face-spec ',class :foreground yellow :weight 'bold))



   ;; Hi-lock
   `(hi-blue (yester-face-spec ',class :foreground blue :inverse-video t))
   `(hi-blue-b (yester-face-spec ',class :foreground blue :weight 'bold))
   `(hi-green (yester-face-spec ',class :foreground green :inverse-video t))
   `(hi-green-b (yester-face-spec ',class :foreground green :weight 'bold))
   `(hi-pink (yester-face-spec ',class :foreground red :inverse-video t))
   `(hi-red-b (yester-face-spec ',class :foreground red :weight 'bold))
   `(hi-yellow (yester-face-spec ',class :foreground yellow :inverse-video t))
   `(hi-aquamarine (yester-face-spec ',class :foreground aqua :inverse-video t))
   `(hi-salmon (yester-face-spec ',class :foreground orange :inverse-video t))



   ;; Pulse
   `(pulse-highlight-start-face
     (yester-face-spec ',class :background selection))



   ;; Epa
   `(epa-mark (yester-face-spec ',class :weight 'bold :foreground orange))
   `(epa-field-name (yester-face-spec ',class :foreground aqua))
   `(epa-field-body (yester-face-spec ',class :foreground comment))
   `(epa-string '((,class :inherit font-lock-string-face)))
   `(epa-validity-disabled (yester-face-spec ',class :foreground red))
   `(epa-validity-high (yester-face-spec ',class :foreground aqua))
   `(epa-validity-low '((,class :slant italic)))
   `(epa-validity-medium '((,class)))



   ;; Speedbar
   `(speedbar-directory-face '((,class :inherit dired-directory)))
   `(speedbar-file-face '((,class)))
   `(speedbar-highlight-face '((,class :inherit highlight  :extend t)))
   `(speedbar-selected-face (yester-face-spec ',class :foreground orange))
   `(speedbar-tag-face (yester-face-spec ',class :foreground green))
   `(speedbar-separator-face (yester-face-spec ',class :foreground purple))
   `(speedbar-button-face (yester-face-spec ',class :foreground yellow))



   ;; Shr
   `(shr-selected-link '((,class :inherit link-visited)))



   ;; Table
   `(table-cell (yester-face-spec ',class :background block))



   ;; Diary
   `(diary (yester-face-spec ',class :foreground yellow))
   `(diary-anniversary (yester-face-spec ',class :foreground purple))
   `(diary-time (yester-face-spec ',class :foreground blue))



   ;; Flymake
   `(flymake-note
     (yester-face-spec ',class :underline `(:style wave :color ,green)))
   `(flymake-warning
     (yester-face-spec ',class :underline `(:style wave :color ,orange)))
   `(flymake-error
     (yester-face-spec ',class :underline `(:style wave :color ,red)))



   ;; Ibuffer
   `(ibuffer-locked-buffer (yester-face-spec ',class :foreground red))



   ;; Cua-mode
   `(cua-global-mark
     (yester-face-spec ',class :foreground orange :inverse-video t))
   `(cua-rectangle (yester-face-spec ',class :background selection))
   `(cua-rectangle-noselect
     (yester-face-spec ',class :background current-line))



   ;; Company
   `(company-echo-common (yester-face-spec ',class :foreground emboss))
   `(company-preview
     (yester-face-spec ',class :foreground comment :underline comment))
   `(company-preview-common '((,class :inherit 'company-preview)))
   `(company-preview-search '((,class :inherit 'company-preview)))
   `(company-scrollbar-bg (yester-face-spec ',class :background current-line))
   `(company-scrollbar-fg (yester-face-spec ',class :background selection))
   `(company-template-field (yester-face-spec ',class :background current-line))
   `(company-tooltip (yester-face-spec ',class :background block))
   `(company-tooltip-annotation
     (yester-face-spec ',class :slant 'italic :foreground comment))
   `(company-tooltip-annotation-selection
     (yester-face-spec ',class :slant 'italic :foreground comment))
   `(company-tooltip-common '((,class)))
   `(company-tooltip-common-selection '((,class)))
   `(company-tooltip-mouse '((,class :inherit highlight)))
   `(company-tooltip-search
     (yester-face-spec ',class :foreground background :background aqua))
   `(company-tooltip-search-selection
     (yester-face-spec ',class :foreground background :background aqua))
   `(company-tooltip-selection (yester-face-spec ',class :background selection))



   ;; Popup
   `(popup-face '((,class :inherit default)))
   `(popup-isearch-match '((,class :inherit (lazy-highlight default))))
   `(popup-menu-mouse-face '((,class :inherit popup-menu-selection-face)))
   `(popup-menu-selection-face
     (yester-face-spec ',class :background selection :inherit 'default))
   `(popup-scroll-bar-background-face
     (yester-face-spec ',class :background current-line))
   `(popup-scroll-bar-foreground-face
     (yester-face-spec ',class :background selection))
   `(popup-summary-face '((,class :inherit (shadow popup-face))))
   `(popup-tip-face (yester-face-spec ',class
                      (night :foreground foreground
                             :background selection
                             :inherit 'popup-face)
                      (day (nil :foreground foreground
                                :background current-line
                                :inherit 'popup-face)
                           (morning :foreground foreground
                                    :background selection
                                    :inherit 'popup-face))))



   ;; Auto-complete
   `(ac-completion-face '((,class :inherit shadow :underline t)))
   `(ac-gtags-candidate-face
     '((t :inherit (font-lock-function-name-face ac-candidate-face))))
   `(ac-gtags-selection-face
     '((t :inherit (font-lock-function-name-face ac-selection-face))))
   `(ac-yasnippet-candidate-face
     '((t :inherit (font-lock-warning-face ac-candidate-face))))
   `(ac-yasnippet-selection-face
     '((t :inherit (font-lock-warning-face ac-selection-face))))



   ;; Visible-mark
   `(visible-mark-active '((,class :inverse-video t)))
   `(visible-mark-face1 '((,class :inherit visible-mark-active)))
   `(visible-mark-face2 '((,class :inherit visible-mark-active)))
   `(visible-mark-forward-face1 '((,class :inherit visible-mark-active)))
   `(visible-mark-forward-face1 '((,class :inherit visible-mark-active)))



   ;; Ace-jump-mode
   `(ace-jump-face-foreground (yester-face-spec ',class
                                (night :foreground emboss)
                                (day :weight 'bold :foreground emboss )))
   `(ace-jump-face-background '((,class :inherit shadow)))



   ;; Volatile-highlights
   `(vhl/default-face '((,class :inherit region)))



   ;; Anzu
   `(anzu-match-1 '((,class :inherit query-replace)))
   `(anzu-match-2 '((,class :inherit query-replace)))
   `(anzu-match-3 '((,class :inherit query-replace)))
   `(anzu-mode-line (yester-face-spec ',class :foreground aqua))
   `(anzu-mode-line-no-match (yester-face-spec ',class :foreground aqua))
   `(anzu-replace-highlight '((,class :inherit lazy-highlight)))
   `(anzu-replace-to (yester-face-spec ',class
                       (night :foreground emboss)
                       (day :weight 'bold :foreground emboss)))



   ;; Helm
   `(helm-action '((,class)))
   `(helm-M-x-key (yester-face-spec ',class :foreground  orange :underline t))
   `(helm-source-header (yester-face-spec ',class
                          :weight 'bold
                          :height 1.4
                          :background current-line
                          :inherit 'variable-pitch))
   `(helm-candidate-number (yester-face-spec ',class
                             (night :foreground yellow)
                             (day (nil :foreground blue :weight 'bold)
                                  (morning :foreground red))))
   `(helm-header-line-left-margin
     (yester-face-spec ',class :foreground yellow :inverse-video t))
   `(helm-selection (yester-face-spec ',class :background selection))
   `(helm-selection-line '((,class :weight bold)))
   `(helm-separator (yester-face-spec ',class
                      (night :foreground yellow)
                      (day (nil :foreground blue :weight 'bold)
                           (morning :foreground red))))
   `(helm-visible-mark (yester-face-spec ',class
                         (night :underline yellow)
                         (day (nil :underline blue)
                              (morning :underline red))))
   `(helm-buffer-archive '((,class :slant italic)))
   `(helm-buffer-directory '((,class :inherit dired-directory)))
   `(helm-buffer-file '((,class)))
   `(helm-buffer-not-saved '((,class :inherit warning)))
   `(helm-buffer-saved-out '((,class :inherit error)))
   `(helm-buffer-modified (yester-face-spec ',class :foreground yellow))
   `(helm-buffer-size '((,class :slant italic :inherit shadow)))
   `(helm-buffer-process (yester-face-spec ',class :foreground aqua))
   `(helm-ff-backup-file '((,class :inherit shadow)))
   `(helm-ff-denied '((,class :slant italic :inherit dired-warning)))
   `(helm-ff-directory '((,class :inherit dired-directory)))
   `(helm-ff-dirs '((,class :inherit dired-directory :inverse-video t)))
   `(helm-ff-dotted-directory
     '((,class :slant italic :inherit helm-ff-directory)))
   `(helm-ff-dotted-symlink-directory
     '((,class :slant italic :inherit helm-ff-symlink)))
   `(helm-ff-executable (yester-face-spec ',class :foreground green))
   `(helm-ff-file '((,class)))
   `(helm-ff-truename '((,class :inherit shadow)))
   `(helm-ff-file-extension '((,class :inherit helm-ff-file)))
   `(helm-ff-invalid-symlink '((,class :inherit dired-warning)))
   `(helm-ff-pipe '((,class :inherit dired-special)))
   `(helm-ff-prefix (yester-face-spec ',class)
                    (night :foreground yellow :weight 'bold)
                    (day (nil :foreground blue :weight 'bold)
                         (morning :foreground red :weight 'bold)))
   `(helm-ff-socket '((,class :slant italic :inherit dired-special)))
   `(helm-ff-suid '((,class :inherit dired-set-id)))
   `(helm-ff-symlink '((,class :inherit dired-symlink)))
   `(helm-bookmark-addressbook (yester-face-spec ',class :foreground orange))
   `(helm-bookmark-file '((,class)))
   `(helm-bookmark-file-not-found '((,class :inherit shadow)))
   `(helm-bookmark-gnus (yester-face-spec ',class :foreground aqua))
   `(helm-bookmark-info (yester-face-spec ',class :foreground purple))
   `(helm-bookmark-man (yester-face-spec ',class :foreground yellow))
   `(helm-bookmark-w3m (yester-face-spec ',class :foreground green))
   `(helm-delete-async-message (yester-face-spec ',class :foreground red))
   `(helm-etags-file (yester-face-spec ',class :foreground green :underline t))
   `(helm-grep-file '((,class :inherit compilation-info)))
   `(helm-grep-finish '((,class :inherit compilation-mode-line-exit)))
   `(helm-grep-lineno '((,class :inherit compilation-line-number)))
   `(helm-grep-match '((,class :inherit helm-match)))
   `(helm-locate-finish '((,class :inherit success)))
   `(helm-match (yester-face-spec ',class
                  (night :foreground emboss)
                  (day :weight 'bold :foreground emboss)))
   `(helm-moccur-buffer (yester-face-spec ',class :foreground green))
   `(helm-resume-need-update (yester-face-spec ',class
                               :weight 'bold
                               :foreground background
                               :background red))
   `(helm-mode-prefix
     (yester-face-spec ',class :foreground yellow :inverse-video t))
   `(helm-prefarg (yester-face-spec ',class :foreground green))



   ;; Helm-swoop
   `(helm-swoop-line-number-face '((,class :inherit shadow)))
   `(helm-swoop-target-line-block-face
     (yester-face-spec ',class :background current-line))
   `(helm-swoop-target-line-face
     (yester-face-spec ',class :background selection))
   `(helm-swoop-target-word-face '((,class :inherit helm-match)))



   ;; Rainbow-delimiters
   `(rainbow-delimiters-base-error-face
     (yester-face-spec ',class
       :foreground red :inherit 'rainbow-delimiters-base-face))
   `(rainbow-delimiters-depth-1-face
     (yester-face-spec ',class :foreground purple))
   `(rainbow-delimiters-depth-2-face
     (yester-face-spec ',class :foreground blue))
   `(rainbow-delimiters-depth-3-face
     (yester-face-spec ',class :foreground aqua))
   `(rainbow-delimiters-depth-4-face
     (yester-face-spec ',class :foreground green))
   `(rainbow-delimiters-depth-5-face
     (yester-face-spec ',class :foreground yellow))
   `(rainbow-delimiters-depth-6-face
     (yester-face-spec ',class :foreground orange))
   `(rainbow-delimiters-depth-7-face
     (yester-face-spec ',class :foreground red))
   `(rainbow-delimiters-depth-8-face
     (yester-face-spec ',class :foreground comment))
   `(rainbow-delimiters-depth-9-face
     (yester-face-spec ',class :foreground foreground))
   `(rainbow-delimiters-unmatched-face
     (yester-face-spec ',class :foreground red :weight 'bold))



   ;; Git-gutter
   `(git-gutter:modified
     (yester-face-spec ',class :foreground diff-accent-yellow))
   `(git-gutter:added (yester-face-spec ',class :foreground diff-accent-green))
   `(git-gutter:deleted (yester-face-spec ',class :foreground diff-accent-red))
   `(git-gutter:separator (yester-face-spec ',class :foreground comment))
   `(git-gutter:unchanged (yester-face-spec ',class :background selection))



   ;; Git-gutter-fringe
   `(git-gutter-fr:modified
     (yester-face-spec ',class :foreground diff-accent-yellow))
   `(git-gutter-fr:added
     (yester-face-spec ',class :foreground diff-accent-green))
   `(git-gutter-fr:deleted
     (yester-face-spec ',class :foreground diff-accent-red))



   ;; Transient
   `(transient-amaranth (yester-face-spec ',class :foreground purple))
   `(transient-blue (yester-face-spec ',class :foreground blue))
   `(transient-pink (yester-face-spec ',class :foreground orange))
   `(transient-red (yester-face-spec ',class :foreground red))
   `(transient-teal (yester-face-spec ',class :foreground aqua))
   `(transient-argument (yester-face-spec ',class :foreground orange))
   `(transient-separator '((,class :inherit mode-line)))
   `(transient-nonstandard-key '((,class :slant italic)))
   `(transient-enabled-suffix (yester-face-spec ',class
                                :weight 'bold
                                :foreground background
                                :background green))
   `(transient-disabled-suffix (yester-face-spec ',class
                                 :weight 'bold
                                 :foreground background
                                 :background red))



   ;; Magit
   `(magit-dimmed '((,class :inherit shadow)))
   `(magit-branch-local (yester-face-spec ',class :foreground aqua))
   `(magit-branch-remote (yester-face-spec ',class :foreground blue))
   `(magit-tag (yester-face-spec ',class :foreground yellow))
   `(magit-hash (yester-face-spec ',class :foreground comment))
   `(magit-header-line (yester-face-spec ',class :foreground purple))
   `(magit-log-author (yester-face-spec ',class :foreground orange))
   `(magit-log-date (yester-face-spec ',class :foreground foreground))
   `(magit-log-graph (yester-face-spec ',class :foreground comment))
   `(magit-section-heading
     (yester-face-spec ',class :foreground yellow :weight 'bold))
   `(magit-section-heading-selection
     (yester-face-spec ',class :background selection))
   `(magit-section-highlight
     (yester-face-spec ',class :background current-line))
   `(magit-process-ng
     (yester-face-spec ',class :foreground red :inherit 'magit-section-heading))
   `(magit-process-ok (yester-face-spec ',class
                        :foreground green :inherit 'magit-section-heading))
   `(magit-sequence-done (yester-face-spec ',class :foreground comment))
   `(magit-sequence-drop (yester-face-spec ',class :foreground red))
   `(magit-sequence-exec (yester-face-spec ',class :foreground comment))
   `(magit-sequence-head (yester-face-spec ',class :foreground aqua))
   `(magit-sequence-onto (yester-face-spec ',class :foreground comment))
   `(magit-sequence-part (yester-face-spec ',class :foreground yellow))
   `(magit-sequence-pick (yester-face-spec ',class :foreground foreground))
   `(magit-sequence-stop (yester-face-spec ',class :foreground blue))
   `(magit-refname (yester-face-spec ',class :foreground foreground))
   `(magit-diff-revision-summary
     (yester-face-spec ',class :weight 'bold :foreground yellow))
   `(magit-diff-context '((,class :inherit diff-context)))
   `(magit-diff-context-highlight (yester-face-spec ',class :background block))
   `(magit-diff-file-heading-selection
     (yester-face-spec ',class :background selection))
   `(magit-diff-hunk-heading '((,class :inherit 'diff-hunk-header)))
   `(magit-diff-hunk-heading-highlight
     (yester-face-spec ',class :background current-line))
   `(magit-diff-hunk-heading-selection
     (yester-face-spec ',class :background selection))
   `(magit-diff-lines-heading (yester-face-spec ',class
                                :background selection
                                :inherit 'magit-diff-hunk-heading-highlight))
   `(magit-diff-lines-boundary
     (yester-face-spec ',class :foreground background :background selection))
   `(magit-diff-added (yester-face-spec ',class
                        (night :foreground diff-variant-green
                               :background diff-green)
                        (day :foreground diff-accent-green
                             :background diff-green)))
   `(magit-diff-added-highlight '((,class :inherit magit-diff-added)))
   `(magit-diff-removed (yester-face-spec ',class
                          (night :foreground diff-variant-red
                                 :background diff-red)
                          (day :foreground diff-accent-red
                               :background diff-red)))
   `(magit-diff-removed-highlight '((,class :inherit magit-diff-removed)))
   `(magit-diff-base (yester-face-spec ',class
                       (night :foreground diff-variant-yellow
                              :background diff-yellow)
                       (day :foreground diff-accent-yellow
                            :background diff-yellow)))
   `(magit-diff-base-highlight '((,class :inherit magit-diff-base)))
   `(magit-diffstat-added
     (yester-face-spec ',class :foreground diff-accent-green))
   `(magit-diffstat-removed
     (yester-face-spec ',class :foreground diff-accent-red))
   `(magit-cherry-equivalent
     (yester-face-spec ',class :foreground diff-accent-cyan))
   `(magit-cherry-unmatched
     (yester-face-spec ',class :foreground diff-accent-yellow))
   `(magit-reflog-amend (yester-face-spec ',class :foreground yellow))
   `(magit-reflog-checkout (yester-face-spec ',class :foreground blue))
   `(magit-reflog-cherry-pick (yester-face-spec ',class :foreground purple))
   `(magit-reflog-commit (yester-face-spec ',class :foreground green))
   `(magit-reflog-merge (yester-face-spec ',class :foreground purple))
   `(magit-reflog-other (yester-face-spec ',class :foreground orange))
   `(magit-reflog-rebase (yester-face-spec ',class :foreground yellow))
   `(magit-reflog-remote (yester-face-spec ',class :foreground aqua))
   `(magit-reflog-reset (yester-face-spec ',class :foreground red))
   `(magit-blame-highlight
     (yester-face-spec ',class :weight 'bold :slant 'italic :background block))
   `(magit-blame-heading (yester-face-spec ',class
                           :weight 'normal
                           :slant 'italic
                           :foreground foreground
                           :background selection))
   `(magit-blame-margin (yester-face-spec ',class
                          :weight 'normal
                          :slant 'normal
                          :foreground foreground
                          :background current-line
                          :underline nil
                          :inverse-video nil))
   `(magit-blame-dimmed (yester-face-spec ',class
                          :weight 'normal
                          :slant 'normal
                          :foreground comment
                          :underline nil
                          :inverse-video nil))
   `(magit-bisect-bad (yester-face-spec ',class :foreground red))
   `(magit-bisect-good (yester-face-spec ',class :foreground green))
   `(magit-bisect-skip (yester-face-spec ',class :foreground orange))
   `(magit-signature-bad
     (yester-face-spec ',class :weight 'bold :foreground red))
   `(magit-signature-error (yester-face-spec ',class :foreground red))
   `(magit-signature-expired (yester-face-spec ',class :foreground orange))
   `(magit-signature-expired-key (yester-face-spec ',class :foreground orange))
   `(magit-signature-good (yester-face-spec ',class :foreground green))
   `(magit-signature-revoked
     (yester-face-spec ',class :weight 'bold :foreground orange))
   `(magit-signature-untrusted (yester-face-spec ',class :foreground purple))



   ;; Git-commit
   `(git-commit-comment-branch-local
     (yester-face-spec ',class :foreground aqua))
   `(git-commit-comment-branch-remote
     (yester-face-spec ',class :foreground blue))



   ;; Flycheck
   `(flycheck-info
     (yester-face-spec ',class :underline `(:style wave :color ,green)))
   `(flycheck-warning
     (yester-face-spec ',class :underline `(:style wave :color ,orange)))
   `(flycheck-error
     (yester-face-spec ',class :underline `(:style wave :color ,red)))



   ;; Dired-hacks
   `(dired-subtree-depth-1-face '((,class)))
   `(dired-subtree-depth-2-face '((,class)))
   `(dired-subtree-depth-3-face '((,class)))
   `(dired-subtree-depth-4-face '((,class)))
   `(dired-subtree-depth-5-face '((,class)))
   `(dired-subtree-depth-6-face '((,class)))



   ;; Evil
   `(evil-ex-info (yester-face-spec ',class :foreground red :slant 'italic))
   `(evil-ex-substitute-matches '((,class :inherit evil-ex-lazy-highlight)))
   `(evil-ex-substitute-replacement (yester-face-spec ',class
                                      (night :foreground emboss)
                                      (day :weight 'bold :foreground emboss)))



   ;; Page-break-lines
   `(page-break-lines (yester-face-spec ',class
                        :foreground selection
                        :distant-foreground background))



   ;; Eglot
   `(eglot-mode-line (yester-face-spec ',class :foreground aqua))



   ;; Markdown-mode
   `(markdown-markup-face '((,class :inherit shadow)))
   `(markdown-footnote-text-face '((,class)))
   `(markdown-gfm-checkbox-face '((,class :weight bold)))
   `(markdown-header-face-1 '((,class :weight bold :inherit outline-1)))
   `(markdown-header-face-2 '((,class :weight bold :inherit outline-2)))
   `(markdown-header-face-3 '((,class :weight bold :inherit outline-3)))
   `(markdown-header-face-4 '((,class :weight bold :inherit outline-4)))
   `(markdown-header-face-5 '((,class :weight bold :inherit outline-5)))
   `(markdown-header-face-6 '((,class :weight bold :inherit outline-6)))
   `(markdown-inline-code-face
     (yester-face-spec ',class :foreground yellow :inherit 'markdown-code-face))
   `(markdown-language-info-face '((,class :inherit markdown-markup-face)))
   `(markdown-language-keyword-face
     '((,class :weight bold :inherit markdown-markup-face)))
   `(markdown-url-face '((,class :inherit shadow)))
   `(markdown-reference-face '((,class :weight bold :inherit shadow)))
   `(markdown-list-face '((,class :weight bold)))
   `(markdown-math-face (yester-face-spec ',class :foreground orange))
   `(markdown-pre-face
     (yester-face-spec ',class :foreground yellow :inherit 'markdown-code-face))
   `(markdown-table-face (yester-face-spec ',class :background block))
   `(markdown-html-attr-name-face
     (yester-face-spec ',class :foreground comment))
   `(markdown-html-attr-value-face (yester-face-spec ',class :foreground green))
   `(markdown-html-entity-face (yester-face-spec ',class :foreground orange))
   `(markdown-html-tag-name-face
     (yester-face-spec ',class :weight 'bold :foreground comment))
   `(markdown-metadata-key-face '((,class :inherit shadow)))
   `(markdown-metadata-value-face (yester-face-spec ',class :foreground aqua))



   ;; Flex-isearch
   `(flex-isearch-message-prefix (yester-face-spec ',class :foreground aqua))



   ;; Wgrep
   `(wgrep-face (yester-face-spec ',class :background selection))
   `(wgrep-done-face '((,class :inherit italic)))
   `(wgrep-reject-face (yester-face-spec ',class :weight 'bold :foreground red))
   `(wgrep-delete-face '((,class :strike-through t)))
   `(wgrep-file-face (yester-face-spec ',class :background selection))



   ;; Smartparens
   `(sp-pair-overlay-face (yester-face-spec ',class :background selection))
   `(sp-wrap-overlay-closing-pair
     (yester-face-spec ',class :foreground red :inherit 'sp-wrap-overlay-face))
   `(sp-wrap-overlay-opening-pair (yester-face-spec ',class
                                    :foreground green
                                    :inherit 'sp-wrap-overlay-face))



   ;; Web-mode
   `(web-mode-error-face '((,class :weight bold :inherit error)))
   `(web-mode-current-column-highlight-face
     (yester-face-spec ',class :background current-line))
   `(web-mode-current-element-highlight-face
     (yester-face-spec ',class :weight 'bold :background current-line))
   `(web-mode-folded-face (yester-face-spec ',class :underline foreground))
   `(web-mode-param-name-face '((,class)))
   `(web-mode-symbol-face (yester-face-spec ',class :foreground aqua))
   `(web-mode-whitespace-face (yester-face-spec ',class
                                :foreground red
                                :background background
                                :inverse-video t))
   `(web-mode-doctype-face '((,class :inherit shadow)))
   `(web-mode-block-face '((,class)))
   `(web-mode-block-attr-name-face
     '((,class :slant italic :inherit web-mode-html-attr-name-face)))
   `(web-mode-block-attr-value-face
     '((,class :slant italic :inherit web-mode-html-attr-value-face)))
   `(web-mode-css-color-face '((,class)))
   `(web-mode-html-attr-name-face '((,class)))
   `(web-mode-html-tag-bracket-face '((,class)))
   `(web-mode-html-tag-face '((,class :inherit shadow)))
   `(web-mode-annotation-face
     '((,class :slant normal :inherit web-mode-comment-face)))
   `(web-mode-comment-keyword-face '((,class :weight bold :underline t)))
   `(web-mode-inlay-face (yester-face-spec ',class :foreground yellow))
   `(web-mode-json-context-face
     (yester-face-spec ',class :slant 'italic :foreground purple))
   `(web-mode-json-key-face (yester-face-spec ',class :foreground purple))
   `(web-mode-jsx-depth-1-face
     (yester-face-spec ',class :background background))
   `(web-mode-jsx-depth-2-face (yester-face-spec ',class :background block))
   `(web-mode-jsx-depth-3-face
     (yester-face-spec ',class :background current-line))
   `(web-mode-jsx-depth-4-face (yester-face-spec ',class :background selection))
   `(web-mode-jsx-depth-5-face
     (yester-face-spec ',class :weight 'bold :background selection))



   ;; YaTeX
   `(delimiter (yester-face-spec ',class :foreground comment))
   `(YaTeX-font-lock-crossref-face (yester-face-spec ',class :foreground blue))
   `(YaTeX-font-lock-declaration-face
     (yester-face-spec ',class :foreground purple))
   `(YaTeX-font-lock-delimiter-face
     (yester-face-spec ',class :foreground orange))
   `(YaTeX-font-lock-formula-face (yester-face-spec ',class :foreground yellow))
   `(YaTeX-font-lock-include-face (yester-face-spec ',class :foreground aqua))
   `(YaTeX-font-lock-label-face (yester-face-spec ',class :foreground green))
   `(YaTeX-font-lock-math-sub-face
     (yester-face-spec ',class :foreground yellow :underline t))
   `(YaTeX-font-lock-math-sup-face
     (yester-face-spec ',class :foreground yellow :overline t))
   `(YaTeX-on-the-fly-activated-face
     (yester-face-spec ',class :background current-line))
   `(YaTeX-sectioning-0 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground green))
   `(YaTeX-sectioning-1 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground blue))
   `(YaTeX-sectioning-2 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground orange))
   `(YaTeX-sectioning-3 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground purple))
   `(YaTeX-sectioning-4 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground comment))
   `(YaTeX-sectioning-5 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground yellow))
   `(YaTeX-sectioning-6 (yester-face-spec ',class
                          :weight 'bold
                          :inverse-video t
                          :foreground aqua))



   ;; Calfw
   `(cfw:face-title (yester-face-spec ',class
                      :weight 'bold
                      :height (* 1.2 1.2 1.2)
                      :foreground foreground
                      :inherit 'variable-pitch))
   `(cfw:face-toolbar
     (yester-face-spec ',class :foreground block :background block))
   `(cfw:face-toolbar-button-on
     (yester-face-spec ',class :foreground orange :background block))
   `(cfw:face-toolbar-button-off
     (yester-face-spec ',class :foreground comment :background block))
   `(cfw:face-disable (yester-face-spec ',class
                        :foreground selection
                        :distant-foreground background))
   `(cfw:face-grid (yester-face-spec ',class
                     :foreground selection
                     :distant-foreground background))
   `(cfw:face-sunday
     (yester-face-spec ',class :weight 'bold :foreground comment))
   `(cfw:face-saturday
     (yester-face-spec ',class :weight 'bold :foreground comment))
   `(cfw:face-header (yester-face-spec ',class :weight 'bold :foreground aqua))
   `(cfw:face-day-title (yester-face-spec ',class :foreground foreground))
   `(cfw:face-holiday (yester-face-spec ',class :foreground purple))
   `(cfw:face-annotation (yester-face-spec ',class :foreground comment))
   `(cfw:face-today-title (yester-face-spec ',class
                            :weight 'bold
                            :foreground yellow
                            :background background
                            :inverse-video t))
   `(cfw:face-select (yester-face-spec ',class :background selection))
   `(cfw:face-default-content (yester-face-spec ',class :foreground foreground))
   `(cfw:face-today (yester-face-spec ',class :foreground yellow))
   `(cfw:face-periods (yester-face-spec ',class :weight 'bold :foreground blue))



   ;; Gnus
   `(gnus-splash (yester-face-spec ',class :foreground selection))
   `(gnus-button '((,class :underline t)))
   `(gnus-header-content
     (yester-face-spec ',class :slant 'italic :foreground comment))
   `(gnus-header-from (yester-face-spec ',class :foreground aqua))
   `(gnus-header-name (yester-face-spec ',class :foreground comment))
   `(gnus-header-newsgroups
     (yester-face-spec ',class :slant 'italic :foreground comment))
   `(gnus-header-subject
     (yester-face-spec ',class :weight 'bold :foreground purple))
   `(gnus-signature '((,class :slant italic)))
   `(gnus-emphasis-highlight-words
     (yester-face-spec ',class :foreground yellow))
   `(gnus-group-mail-1
     (yester-face-spec ',class :weight 'bold :foreground aqua))
   `(gnus-group-mail-1-empty
     '((,class :slant italic :inherit gnus-group-mail-1)))
   `(gnus-group-mail-2 (yester-face-spec ',class :foreground aqua))
   `(gnus-group-mail-2-empty
     '((,class :slant italic :inherit gnus-group-mail-2)))
   `(gnus-group-mail-3 '((,class)))
   `(gnus-group-mail-3-empty
     '((,class :slant italic :inherit gnus-group-mail-3)))
   `(gnus-group-mail-low (yester-face-spec ',class :foreground comment))
   `(gnus-group-mail-low-empty
     '((,class :slant italic :inherit gnus-group-mail-low)))
   `(gnus-group-news-1
     (yester-face-spec ',class :weight 'bold :foreground aqua))
   `(gnus-group-news-1-empty
     '((,class :slant italic :inherit gnus-group-news-1)))
   `(gnus-group-news-2 (yester-face-spec ',class :foreground aqua))
   `(gnus-group-news-2-empty
     '((,class :slant italic :inherit gnus-group-news-2)))
   `(gnus-group-news-3 '((,class)))
   `(gnus-group-news-3-empty
     '((,class :slant italic :inherit gnus-group-news-3)))
   `(gnus-group-news-4 '((,class)))
   `(gnus-group-news-4-empty
     '((,class :slant italic :inherit gnus-group-news-4)))
   `(gnus-group-news-5 '((,class)))
   `(gnus-group-news-5-empty
     '((,class :slant italic :inherit gnus-group-news-5)))
   `(gnus-group-news-6 (yester-face-spec ',class :foreground comment))
   `(gnus-group-news-6-empty
     '((,class :slant italic :inherit gnus-group-news-6)))
   `(gnus-group-news-low (yester-face-spec ',class :foreground comment))
   `(gnus-group-news-low-empty
     '((,class :slant italic :inherit gnus-group-news-low)))
   `(gnus-summary-selected (yester-face-spec ',class :background selection))
   `(gnus-summary-normal-ancient (yester-face-spec ',class :foreground blue))
   `(gnus-summary-normal-read (yester-face-spec ',class :foreground green))
   `(gnus-summary-normal-ticked (yester-face-spec ',class :foreground orange))
   `(gnus-summary-normal-undownloaded
     (yester-face-spec ',class :foreground comment))
   `(gnus-summary-cancelled (yester-face-spec ',class :foreground yellow))
   `(gnus-server-denied
     (yester-face-spec ',class :weight 'bold :foreground red))
   `(gnus-server-offline
     (yester-face-spec ',class :weight 'bold :foreground orange))
   `(gnus-server-opened
     (yester-face-spec ',class :weight 'bold :foreground green))
   `(gnus-server-closed (yester-face-spec ',class :foreground comment))
   `(gnus-server-agent
     (yester-face-spec ',class :weight 'bold :foreground blue))
   `(gnus-server-cloud
     (yester-face-spec ',class :weight 'bold :foreground aqua))
   `(gnus-server-cloud-host (yester-face-spec ',class
                              :weight 'bold
                              :foreground aqua
                              :inverse-video t))
   `(gnus-cite-1 (yester-face-spec ',class :foreground purple))
   `(gnus-cite-2 (yester-face-spec ',class :foreground blue))
   `(gnus-cite-3 (yester-face-spec ',class :foreground aqua))
   `(gnus-cite-4 (yester-face-spec ',class :foreground green))
   `(gnus-cite-5 (yester-face-spec ',class :foreground yellow))
   `(gnus-cite-6 (yester-face-spec ',class :foreground orange))
   `(gnus-cite-7 (yester-face-spec ',class :foreground red))
   `(gnus-cite-8 (yester-face-spec ',class :foreground comment))
   `(gnus-cite-9 (yester-face-spec ',class :foreground comment))
   `(gnus-cite-10 (yester-face-spec ',class :foreground comment))
   `(gnus-cite-11 (yester-face-spec ',class :foreground comment))
   ;; Message
   `(message-cited-text-1 (yester-face-spec ',class :foreground purple))
   `(message-cited-text-2 (yester-face-spec ',class :foreground blue))
   `(message-cited-text-3 (yester-face-spec ',class :foreground aqua))
   `(message-cited-text-4 (yester-face-spec ',class :foreground green))
   `(message-header-cc
     (yester-face-spec ',class :slant 'italic :foreground aqua))
   `(message-header-name (yester-face-spec ',class :foreground comment))
   `(message-header-newsgroups (yester-face-spec ',class :foreground blue))
   `(message-header-other
     (yester-face-spec ',class :slant 'italic :foreground comment))
   `(message-header-subject
     (yester-face-spec ',class :weight 'bold :foreground purple))
   `(message-header-to (yester-face-spec ',class :foreground aqua))
   `(message-header-xheader (yester-face-spec ',class :foreground green))
   `(message-mml (yester-face-spec ',class :foreground yellow))
   `(message-separator
     (yester-face-spec ',class :slant 'italic :foreground comment))
   ;; Mm-decode
   `(mm-command-output (yester-face-spec ',class :foreground green))
   ;; Mm-uu
   `(mm-uu-extract
     (yester-face-spec ',class :foreground green :inverse-video t))



   ;; Skk
   `(skk-treat-default (yester-face-spec ',class :foreground foreground))
   `(skk-henkan-face-default
     (yester-face-spec ',class :foreground background :background aqua))
   `(skk-prefix-hiragana-face (yester-face-spec ',class :foreground blue))
   `(skk-prefix-jisx0201-face (yester-face-spec ',class :foreground comment))
   `(skk-prefix-katakana-face (yester-face-spec ',class :foreground aqua))
   `(skk-dcomp-face (yester-face-spec ',class :foreground comment :underline t))
   `(skk-dcomp-multiple-face (yester-face-spec ',class :foreground foreground))
   `(skk-dcomp-multiple-selected-face
     (yester-face-spec ',class :foreground foreground :background selection))
   `(skk-dcomp-multiple-trailing-face
     (yester-face-spec ',class :foreground comment))
   `(skk-display-code-char-face
     (yester-face-spec ',class :weight 'bold :foreground blue))
   `(skk-display-code-prompt-face
     (yester-face-spec ',class :foreground aqua))
   `(skk-henkan-show-candidates-buffer-anno-face
     (yester-face-spec ',class :foreground comment))
   `(skk-henkan-show-candidates-buffer-cand-face
     (yester-face-spec ',class :foreground foreground))
   `(skk-henkan-show-candidates-keys-face
     (yester-face-spec ',class :foreground aqua))
   `(skk-inline-show-vertically-anno-face
     (yester-face-spec ',class :foreground comment))
   `(skk-inline-show-vertically-cand-face
     (yester-face-spec ',class :foreground foreground))
   `(skk-tooltip-show-at-point-anno-face
     (yester-face-spec ',class :foreground comment))
   `(skk-tooltip-show-at-point-cand-face
     (yester-face-spec ',class :foreground foreground))
   `(skk-jisyo-registration-badge-face
     (yester-face-spec ',class :foreground background :background blue))
   `(skk-list-chars-table-header-face
     (yester-face-spec ',class :foreground comment))
   `(skk-show-mode-inline-face
     (yester-face-spec ',class :background selection :box t))
   `(skk-tankan-radical-name-face
     (yester-face-spec ',class :foreground comment))
   `(skk-verbose-intention-face (yester-face-spec ',class :foreground comment))
   `(skk-verbose-kbd-face (yester-face-spec ',class :foreground aqua))
   `(skk-tut-do-it-face (yester-face-spec ',class :foreground yellow))
   `(skk-tut-hint-face (yester-face-spec ',class :foreground aqua))
   `(skk-tut-key-bind-face (yester-face-spec ',class :foreground comment))
   `(skk-tut-question-face (yester-face-spec ',class :foreground green))
   `(skk-tut-section-face
     (yester-face-spec ',class :foreground purple :inverse-video t))



   ;; Japanese-holidays
   `(japanese-holiday-saturday (yester-face-spec ',class :foreground comment))))



(yester-theme-set-variables
 'yester

 ;; Distant-foreground
 '(face-near-same-color-threshold (yester-symbol-exp
                                    (night 5000)
                                    (day (nil 10000)
                                         (morning 3000))))



 ;; Popup
 `(popup-isearch-cursor-color (yester-symbol-exp yellow))



 ;; Auto-complete
 `(ac-fuzzy-cursor-color (yester-symbol-exp orange))



 ;; Smartrep
 `(smartrep-mode-line-active-bg (yester-symbol-exp selection))



 ;; Evil
 `(evil-emacs-state-cursor (yester-symbol-exp red))
 `(evil-normal-state-cursor (yester-symbol-exp green))
 `(evil-insert-state-cursor (yester-symbol-exp `(bar ,green)))
 `(evil-replace-state-cursor (yester-symbol-exp `(hbar ,green)))
 `(evil-operator-state-cursor (yester-symbol-exp green))
 `(evil-visual-state-cursor (yester-symbol-exp green))
 `(evil-motion-state-cursor (yester-symbol-exp comment))



 ;; Calfw
 `(cfw:face-item-separator-color (yester-symbol-exp selection))
 `(cfw:org-face-agenda-item-foreground-color (yester-symbol-exp foreground))



 ;; Gnus
 `(gnus-logo-colors (yester-symbol-exp (list selection current-line)))



 ;; Skk
 `(skk-use-color-cursor t)
 `(skk-cursor-abbrev-color (yester-symbol-exp blue))
 `(skk-cursor-jisx0208-latin-color (yester-symbol-exp blue))
 `(skk-cursor-katakana-color (yester-symbol-exp aqua))
 `(skk-cursor-hiragana-color (yester-symbol-exp blue))
 `(skk-cursor-jisx0201-color (yester-symbol-exp aqua))
 `(skk-cursor-latin-color (yester-symbol-exp red))
 `(skk-inline-show-background-color (yester-symbol-exp current-line))
 `(skk-inline-show-background-color-odd (yester-symbol-exp current-line)))

(provide-theme 'yester)

;;; yester-theme.el ends here
