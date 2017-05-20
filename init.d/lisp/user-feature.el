
;;;; user-feature.el


(mapc (lambda (e) (add-to-list 'load-path e))
      (reverse
       (letrec ((filter
                 (lambda (p l)
                   (cond ((null l) l)
                         ((funcall p (car l)) (funcall filter p (cdr l)))
                         (t (cons (car l) (funcall filter p (cdr l))))))))
         (apply 'append
                (mapcar
                 (lambda (d)
                   (funcall
                    filter
                    (lambda (f) (not (file-directory-p f)))
                    (mapcar (lambda (f) (expand-file-name (concat d "/" f)))
                            (remove ".." (directory-files d)))))
                 '("~/.emacs.d/site-lisp" "~/.emacs.d/lisp"))))))

(require 'user-feature-loaddefs nil 'noerror)

(mapc
 (lambda (el)
   (let ((elc (concat el "c")))
     (unless (and (file-exists-p elc) (not (file-writable-p elc)))
       (byte-compile-file el))))
 (directory-files-recursively "~/.emacs.d/lisp/" "\\.el$"))


(resolve user-feature)
