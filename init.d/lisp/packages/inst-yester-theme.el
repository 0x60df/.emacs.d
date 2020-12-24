
;;;; inst-yester-theme.el


(premise init)

(let* ((name "yester")
       (directory (concat user-emacs-directory "package/" name "-theme")))
  (add-to-list 'load-path directory)

  (let ((el (locate-file name load-path '(".el")))
        (elc (locate-file name load-path '(".elc"))))
    (if (null el)
        (error "Cannot find pacakge `%s'" name)
      (unless (and elc (file-newer-than-file-p elc el))
        (if elc (and (delete-file elc) (setq elc nil)))
        (and (byte-compile-file el) (setq elc (byte-compile-dest-file el))))
      (if (null elc)
          (error "Connot byte compile pacakge `%s'" name))))

  (add-to-list 'custom-theme-load-path directory))


(resolve inst-yester-theme)
