
;;;; files.el


(premise init)
(premise feature)

(lazy-autoload 'seq-difference "seq")

(defun directory-directories-recursively (dir regexp
                                              &optional
                                              predicate
                                              follow-symlinks)
  "Return list of directories by recursive search.
Arguments have same meaning as `directory-files-recursively'."
  (seq-difference
   (directory-files-recursively dir regexp t predicate follow-symlinks)
   (directory-files-recursively dir regexp nil predicate follow-symlinks)))


(resolve files)
