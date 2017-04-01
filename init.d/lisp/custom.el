
;;;; custom.el


(defgroup user nil "" :group 'emacs)

(custom-set-variables '(custom-file "~/.emacs.d/custom.el"))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(defun custom-clean-settings ()
  (interactive)
  (if (and custom-file (file-exists-p custom-file))
      (delete-file custom-file)))


(resolve custom)
