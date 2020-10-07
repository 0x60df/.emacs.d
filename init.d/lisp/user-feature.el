
;;;; user-feature.el


(premise init)

(mapc (lambda (e) (add-to-list 'load-path e))
      (reverse
       (letrec ((list-directories-recursively
                 (lambda (l)
                   (cond ((null l) l)
                         ((file-directory-p (car l))
                          (append
                           (cons (car l)
                                 (funcall
                                  list-directories-recursively
                                  (directory-files
                                   (car l) t "[^.]$\\|[^./]\\.$\\|[^/]\\.\\.")))
                           (funcall list-directories-recursively (cdr l))))
                         (t (funcall list-directories-recursively (cdr l)))))))
         (apply 'append
                (mapcar
                 (lambda (directory-relative-name)
                   (let ((directory-absolute-name
                          (expand-file-name (concat user-emacs-directory
                                                    directory-relative-name))))
                     (cons directory-absolute-name
                           (funcall list-directories-recursively
                                    (directory-files
                                     directory-absolute-name
                                     t "[^.]$\\|[^./]\\.$\\|[^/]\\.\\.")))))
                 '("site-lisp" "lisp"))))))

(require 'user-feature-loaddefs nil 'noerror)

(mapc
 (lambda (el)
   (let ((elc (concat el "c")))
     (unless (and (file-exists-p elc) (file-newer-than-file-p elc el))
       (byte-compile-file el))))
 (apply 'append
        (mapcar
         (lambda (d)
           (directory-files-recursively
            (expand-file-name (concat user-emacs-directory d))
            "\\.el$"))
         '("site-lisp" "lisp"))))


(resolve user-feature)
