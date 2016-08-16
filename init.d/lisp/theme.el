
;;;; theme.el


(setq custom-theme-directory "~/.emacs.d/themes/")

(defun put-on (theme)
  (interactive
   (list (intern (completing-read "Custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (if (custom-theme-p theme)
      (enable-theme theme)
    (load-theme theme t t)
    (enable-theme theme)))

(defalias 'take-off 'disable-theme)
