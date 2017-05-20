
;;;; custom.el


(premise init)

(defgroup user nil "" :group 'emacs)

(custom-set-variables '(custom-file (concat user-emacs-directory "custom.el")))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(defun custom-clean-settings ()
  (interactive)
  (if (and custom-file (file-exists-p custom-file))
      (delete-file custom-file)))


(resolve custom)
