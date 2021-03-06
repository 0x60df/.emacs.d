;;; yester.el --- Feature for yester theme, derived from tomorrow theme

;; Copyright (C) 2020 0x60DF

;;; Commentary:

;; Color definitions and utility functions

;; Color scheme is designed by Chris Kempson:
;; https://github.com/chriskempson/tomorrow-theme

;;; Code:

(defgroup yester nil "Yester theme." :group 'faces)

(defconst yester-colors
  '((night . ((background . "#1d1f21")
              (current-line . "#282a2e")
              (selection . "#373b41")
              (foreground . "#c5c8c6")
              (comment . "#969896")
              (red . "#cc6666")
              (orange . "#de935f")
              (yellow . "#f0c674")
              (green . "#b5bd68")
              (aqua . "#8abeb7")
              (blue . "#81a2be")
              (purple . "#b294bb")))
    (day . ((background . "#ffffff")
            (current-line . "#efefef")
            (selection . "#d6d6d6")
            (foreground . "#4d4d4c")
            (comment . "#8e908c")
            (red . "#c82829")
            (orange . "#f5871f")
            (yellow . "#eab700")
            (green . "#718c00")
            (aqua . "#3e999f")
            (blue . "#4271ae")
            (purple . "#8959a8"))))
  "Colors for `yester-theme'.")

(defconst yester-extended-colors
  '((night . ((block . "#222427")
              (emboss . "#ffd700")
              (diff-green . "#335533")
              (diff-red . "#553333")
              (diff-yellow . "#524a32")
              (diff-cyan . "#305454")
              (diff-variant-green . "#99cc99")
              (diff-variant-red . "#f5a2a6")
              (diff-variant-yellow . "#ffda5e")
              (diff-variant-cyan . "#a1e2e2")
              (diff-accent-green . "#048900")
              (diff-accent-red . "#c82829")
              (diff-accent-yellow . "#d6a400")
              (diff-accent-cyan . "#3d999e")))
    (day . ((block ."#f7f7f7")
            (emboss . "#0f52ba")
            (diff-green . "#eeffee")
            (diff-red . "#ffeeee")
            (diff-yellow . "#fcf9eb")
            (diff-cyan . "#e9fcfc")
            (diff-variant-green . "#cbf5cb")
            (diff-variant-red . "#ffd4d6")
            (diff-variant-yellow . "#fff1d6")
            (diff-variant-cyan . "#d0f2f2")
            (diff-accent-green . "#00a300")
            (diff-accent-red . "#e63535")
            (diff-accent-yellow . "#f5c000")
            (diff-accent-cyan . "#46bac2"))))
  "Colors for extended use for `yester-theme'.")

(defconst yester-scene-colors
  '((night . nil)
    (day . ((morning . ((background . "#f7f5ed")
                        (current-line . "#eceae0")
                        (selection . "#e2dfd4")
                        (block . "#f2f0e7")
                        (emboss . "#e34234")
                        (diff-green . "#e1f2e1")
                        (diff-red . "#f7e6e6")
                        (diff-yellow . "#f7f0d5")
                        (diff-cyan . "#dcf2f2")
                        (diff-variant-green . "#c5edc5")
                        (diff-variant-red . "#f7cdcf")
                        (diff-variant-yellow . "#fcedbb")
                        (diff-variant-cyan . "#c7ebeb")
                        (diff-accent-green . "#009600")
                        (diff-accent-red . "#e03131")
                        (diff-accent-yellow . "#f7b900")
                        (diff-accent-cyan . "#44b5bd"))))))
  "Colors for specific scene for `yester-theme'.")

(defcustom yester-scene nil
  "Scene specifier for night and day phase of `yester-theme'.
Value of this user option is alist, and looks like
(('night . scene) ('day . scene)).
If either night or day cell is omitted, or specified scene
does not exist, no scene is applied for that phase.
Furthermore, multiple cells can be added to this variable.
They are dealt with in the standard manner for alist.
Therefore, the first cell for each phase is employed.
If scene is applied, scene colors shadow phase colors."
  :type '(alist :key-type (choice (const night) (const day)) :key-value symbol)
  :group 'yester)

(defcustom yester-recalc '(yester)
  "List of themes which will be recalculated when phase is shifted."
  :type '(repeat symbol)
  :group 'yester)

(defmacro yester-let-colors (phase &rest body)
  "Eval BODY with let of color name with hex code for yester theme.
PHASE must be either night or day, which specifies
which set of colors is referred."
  (declare (indent 1))
  (let ((colors (if (memq phase '(night day))
                    (append (cdr (assq (cdr (assq phase yester-scene))
                                       (cdr (assq phase yester-scene-colors))))
                            (cdr (assq phase yester-colors))
                            (cdr (assq phase yester-extended-colors)))
                  (error "Invalid phase `%s' for yester theme" phase)))
        (varlist nil))
    (mapc
     (lambda (name-hex)
       (let ((name (car name-hex))
             (hex (cdr name-hex)))
         (unless (assq name varlist)
           (setq varlist (cons `(,name ,hex) varlist)))))
     colors)
    `(let ,varlist
       ,@body)))

(defmacro yester-whole-face-spec (display &rest plist)
  "Construct face spec form for day and night by DISPLAY and PLIST.
DISPLAY must be a list, because background-mode is valid
only with a display which is specified by a list.
PLIST must be specified as rest arguments."
  (declare (indent 1))
  `(if (listp ,display)
       (list
        (cons (append ,display '((background dark)))
              (yester-let-colors night ,(cons 'list plist)))
        (cons (append ,display '((background light)))
              (yester-let-colors day ,(cons 'list plist))))
     (error "Display `%s' is not supported" ,display)))

(defmacro yester-whole-symbol-exp (form)
  "Construct expression for day and night symbol value by FORM."
  `(list
    'cond
    (list '(eq frame-background-mode 'dark)
          (list 'quote (yester-let-colors night ,form)))
    (list '(eq frame-background-mode 'light)
          (list 'quote (yester-let-colors day ,form)))
    (list t (list 'cond
                  (list '(eq (frame-parameter nil 'background-mode) 'dark)
                        (list 'quote (yester-let-colors night ,form)))
                  (list '(eq (frame-parameter nil 'background-mode) 'light)
                        (list 'quote (yester-let-colors day ,form)))))))

(defun yester-recalc ()
  "Recalc faces and variables for THEME."
  (mapc (lambda (theme)
          (dolist (setting (get theme 'theme-settings))
            (let ((prop (car setting))
                  (symbol (cadr setting)))
              (cond ((eq prop 'theme-face)
                     (custom-theme-recalc-face symbol))
                    ((eq prop 'theme-value)
                     (custom-theme-recalc-variable symbol))))))
        yester-recalc))

(defun yester--recalc-once-on-post-command ()
  "Recalc once and `remove-hook' `post-command-hook'."
  (yester-recalc)
  (remove-hook 'post-command-hook #'yester--recalc-once-on-post-command))

(defun yester--background-mode-guard (symbol newval operation where)
  "Watch `frame-background-mode' and `add-hook' `post-command-hook'."
  (if (eq operation 'set)
      (add-hook 'post-command-hook #'yester--recalc-once-on-post-command)))

(define-minor-mode yester-phase-shift-mode
  "Minor mode for supporting phase shift for `yester-theme'."
  :group 'yester
  :global t
  (if yester-phase-shift-mode
      (add-variable-watcher 'frame-background-mode
                            #'yester--background-mode-guard)
    (remove-variable-watcher 'frame-background-mode
                             #'yester--background-mode-guard)))

(provide 'yester)

;;; yester.el ends here
