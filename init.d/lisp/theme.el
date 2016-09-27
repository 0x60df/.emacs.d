
;;;; theme.el


(custom-set-variables '(custom-theme-directory "~/.emacs.d/themes/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/site-lisp")

(defun put-on (theme)
  (interactive
   (list (intern (completing-read "Custom theme: "
                                  (mapcar 'symbol-name
                                          (custom-available-themes))))))
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
                   (not spec-list))
              ;; put original propery
              (put symbol 'saved-symbol-value (symbol-value symbol)))))))
  (enable-theme theme))

(defun take-off (theme)
  (interactive (list (intern
		      (completing-read
		       "Disable custom theme: "
		       (mapcar 'symbol-name custom-enabled-themes)
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
                 (not val))
            (set symbol (get symbol 'saved-symbol-value)))))))
