
;;;; theme.el


(premise init)
(premise custom)

(custom-set-variables
 '(custom-theme-directory (concat user-emacs-directory "themes/")))

(with-eval-after-load 'custom
  (add-to-list 'custom-theme-load-path
               (concat user-emacs-directory "themes/lisp"))
  (add-to-list 'custom-theme-load-path
               (concat user-emacs-directory "themes/site-lisp")))



(defun catch-up-theme-value (symbol)
  "Set SYMBOL which is defined by `defvar' as theme value.
If SYMBOL has not been defined or is custom variable, this
function do nothing."
  (if (and (boundp symbol) (not (custom-variable-p symbol)))
      (let ((saved-value (car (get symbol 'saved-value)))
            (saved-symbol-value (get symbol 'saved-symbol-value)))
        (when (and saved-value (not saved-symbol-value))
          (put symbol 'saved-symbol-value (list (symbol-value symbol)))
          (set symbol (eval saved-value))))))

(defun put-on (theme)
  "Load THEME if not loaded and enable that one.
If theme-settings contains theme-value for
non-custom-variable, this function may save pseudo standard
value for that variable as symbol property. Pseudo standard
value named saved-symbol-value is saved if variable is
bound, is not set by any other theme, and does not have the
symbol property saved-symbol-value."
  (interactive
   (list (intern (completing-read "Enable custom theme: "
                                  (mapcar #'symbol-name
                                          (custom-available-themes))
                                  nil t))))
  (unless (custom-theme-p theme)
    (load-theme theme t t)
    (let ((settings (get theme 'theme-settings)))
      (dolist (s settings)
        (let* ((prop (car s))
               (symbol (cadr s))
               (spec-list (get symbol prop)))
          (if (and (eq prop 'theme-value)
                   (boundp symbol)
                   (not (custom-variable-p symbol))
                   (not spec-list)
                   (not (plist-member
                         (symbol-plist symbol) 'saved-symbol-value)))
              (put symbol 'saved-symbol-value (list (symbol-value symbol))))))))
  (enable-theme theme))

(defun take-off (theme)
  "Disable THEME.
If theme-settings contains theme-value for
non-custom-variable, this function may restore pseudo
standard value for that variable. Pseudo standard value
saved as symbol property saved-symbol-value is restored if
variable is bound, is not set by any other theme, and have
the symbol property saved-symbol-value."
  (interactive
   (list (intern (completing-read "Disable custom theme: "
		                  (mapcar #'symbol-name custom-enabled-themes)
		                  nil t))))
  (disable-theme theme)
  (let ((settings (get theme 'theme-settings)))
    (dolist (s settings)
      (let* ((prop (car s))
             (symbol (cadr s))
             (val (get symbol prop)))
        (if (and (eq prop 'theme-value)
                 (boundp symbol)
                 (not (custom-variable-p symbol))
                 (not val)
                 (plist-member (symbol-plist symbol) 'saved-symbol-value))
            (set symbol (car (get symbol 'saved-symbol-value))))))))


(resolve theme)
