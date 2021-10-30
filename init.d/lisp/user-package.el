
;;;; user-package.el


(premise init)
(premise feature)

(lazy-autoload 'lm-with-file "lisp-mnt")
(lazy-autoload 'lm-header-multiline "lisp-mnt")

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
                 (if (not (or (locate-file name load-path (get-load-suffixes))
                              (package-installed-p package)))
                     (package-install package))))
             requires))))))


(resolve user-package)
