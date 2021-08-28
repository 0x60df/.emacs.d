;;; yester.el --- Feature for yester theme, derived from tomorrow theme

;; Copyright (C) 2020 0x60DF

;;; Commentary:

;; Color definitions and utility functions

;; Base color scheme is designed by Chris Kempson:
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

(defconst yester-scenes
  '((night . nil)
    (day . (morning)))
  "Scene list for night and day phase.")

(defvar yester--scene '((night . nil) (day . nil))
  "Scene specifier for night and day phase of `yester-theme'.
This looks like ((night . SCENE) (day . SCENE)).
If SCENE is nil as a default value, no scene colors and
scene forms are applied.
If scene is applied, scene colors shadow phase colors, and
scene forms overwrite face specs and variable expressions.")

(defvar yester--themes nil
  "Themes of `yester-theme' and its settings.
This looks like below.

((THEME . ((face . (SETTINGS...))
           (variable . (SETTINGS...))))...)

SETTINGS for face and variable should be valid list for
`custom-theme-set-faces' and `custom-theme-set-variables' as
arguments but second form for each SETTINGS should be a raw
form which will return face spec or variable expression when
evaluated.  Every time scene is changed, this second form
will be evaluated for setting scene based face specs and
variable expressions.
THEME is arbitrary theme name.")
(put 'yester-forms 'risky-local-variable t)

(defcustom yester-recalc '(yester)
  "List of themes which will be recalculated when phase is shifted."
  :type '(repeat symbol)
  :group 'yester)

(defun yester-current-phase ()
  "Return current phase.
If no phase is specified, return nil."
  (cond ((eq frame-background-mode 'dark) 'night)
        ((eq frame-background-mode 'light) 'day)
        ((eq (frame-parameter nil 'background-mode) 'dark) 'night)
        ((eq (frame-parameter nil 'background-mode) 'light) 'day)))

(defun yester-current-scene (&optional phase)
  "Return current scene of PHASE.
If PHASE is omitted, use current phase."
  (cdr (assq (or phase (yester-current-phase)) yester--scene)))

(defmacro yester-let-colors (phase &rest body)
  "Eval BODY with let of color name with hex code for yester theme.
PHASE must be either night or day, which specifies
which set of colors is referred."
  (declare (indent 1))
  (let ((colors (if (memq phase '(night day))
                    (append (cdr (assq (cdr (assq phase yester--scene))
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

(defun yester-theme-set-faces (theme &rest args)
  "Set face specs for THEME by using ARGS.
Themes who defines yester themes should use this functions
instead of `custom-theme-set-faces'.
The arguments should fit with `custom-theme-set-faces' but
second entry for each element of ARGS should be a raw form
which will return face spec when evaluated.
ARGS are saved in `yester--themes' and every time scene is
changed, second raw form will be evaluated for setting scene
based face spec."
  (unless (assq theme yester--themes)
    (push (cons theme nil) yester--themes))
  (let ((cell (assq 'face (cdr (assq theme yester--themes)))))
    (if cell
        (setcdr (assq 'face (cdr (assq theme yester--themes))) args))
    (push (cons 'face args) (cdr (assq theme yester--themes))))
  (apply #'custom-theme-set-faces theme
         (mapcar (lambda (form)
                   (unless (and (listp form) (< 1 (length form)))
                     (error "Invalid for setting face spec: %s" form))
                   `(,(car form)
                     ,(eval (cadr form))
                     ,@(cddr form)))
                 args)))

(defun yester-theme-set-variables (theme &rest args)
  "Set variable expressions for THEME by using ARGS.
Themes who defines yester themes should use this functions
instead of `custom-theme-set-variables'.
The arguments should fit with `custom-theme-set-variables'
but second entry for each element of ARGS should be a raw
form which will return variable expression when evaluated.
ARGS are saved in `yester--themes' and every time scene is
changed, second raw form will be evaluated for setting scene
based variable expression."
  (unless (assq theme yester--themes)
    (push (cons theme nil) yester--themes))
  (let ((cell (assq 'variable (cdr (assq theme yester--themes)))))
    (if cell
        (setcdr (assq 'variable (cdr (assq theme yester--themes))) args))
    (push (cons 'variable args) (cdr (assq theme yester--themes))))
  (apply #'custom-theme-set-variables theme
         (mapcar (lambda (form)
                   (unless (and (listp form) (< 1 (length form)))
                     (error "Invalid for setting variable expression : %s"
                            form))
                   `(,(car form)
                     ,(eval (cadr form))
                     ,@(cddr form)))
                 args)))

(defun yester-change-scene (scene phase)
  "Change scene of PHASE to SCENE.
When called interactively, use current-phase as PHASE."
  (interactive (let ((phase (yester-current-phase)))
                 (list
                  (intern
                   (completing-read "Scene: "
                                    (remq
                                     (cdr (assq phase yester--scene))
                                     (cons nil
                                           (cdr (assq phase yester-scenes))))
                                    nil t))
                  phase)))
  (unless (eq (cdr (assq phase yester--scene)) scene)
    (setcdr (assq phase yester--scene) scene)
    (dolist (theme-forms yester--themes)
      (apply #'custom-theme-set-faces (car theme-forms)
               (mapcar (lambda (form)
                         (unless (and (listp form) (< 1 (length form)))
                           (error "Invalid for setting face spec: %s" form))
                         `(,(car form)
                           ,(eval (cadr form))
                           ,@(cddr form)))
                       (cdr (assq 'face (cdr theme-forms)))))
      (apply #'custom-theme-set-variables (car theme-forms)
               (mapcar (lambda (form)
                         (unless (and (listp form) (< 1 (length form)))
                           (error "Invalid for setting variable expression : %s"
                                  form))
                         `(,(car form)
                           ,(eval (cadr form))
                           ,@(cddr form)))
                       (cdr (assq 'variable (cdr theme-forms))))))
    (yester-recalc)))

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
