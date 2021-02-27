
;;;; user-package.el


(premise init)
(premise init-el-get)

(defconst user-package-directory (concat user-emacs-directory "uelpa/")
  "Directory which contains user packages.")

(defun user-package-resolve-requires (file)
  "Resolve package-requires of FILE."
  (lm-with-file file
    (let ((header (lm-header-multiline "package-requires")))
      (if header
          (let ((requires (read (mapconcat #'identity header " "))))
            (mapc
             (lambda (spec)
               (let* ((package (cond
                                ((listp spec) (car spec))
                                ((symbolp spec) spec)
                                (t (error "Invalid requires spec `%s'"
                                          spec))))
                      (name (symbol-name package)))
                 (when (or (not (locate-file name load-path
                                             (get-load-suffixes)))
                           (el-get-package-installed-p name))
                   (eval `(el-get-bundle ,package)))))
             requires))))))


(resolve user-package)
