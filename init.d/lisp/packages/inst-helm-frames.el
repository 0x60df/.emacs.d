
;;;; inst-helm-frames.el


(premise init)
(premise inst-helm)

(let* ((name "helm-frames")
       (directory (concat user-emacs-directory "package/" name)))
  (add-to-list 'load-path directory)

  (let ((el (locate-file name load-path '(".el")))
        (elc (locate-file name load-path '(".elc"))))
    (if (null el)
        (error "Cannot find pacakge `%s'" name)
      (unless (and elc (file-newer-than-file-p elc el))
        (if elc (and (delete-file elc) (setq elc nil)))
        (and (byte-compile-file el) (setq elc (byte-compile-dest-file el))))
      (if (null elc)
          (error "Connot byte compile pacakge `%s'" name))
      (let ((loaddefs (replace-regexp-in-string "\\.el$" "-loaddefs.el" el)))
        (unless (and (file-exists-p loaddefs)
                     (file-newer-than-file-p loaddefs el))
          (if (file-exists-p loaddefs) (delete-file loaddefs))
          (update-file-autoloads el t loaddefs))
        (unless (file-exists-p loaddefs)
          (error "Connot update loaddefs for pacakge `%s'" name)))))

  (require 'helm-frames-loaddefs))


(resolve inst-helm-frames)
