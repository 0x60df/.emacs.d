
;;;; user-feature.el


(premise init)
(premise files)

(require 'seq)

(defconst user-feature-directory (concat user-emacs-directory "lisp/")
  "Directory which contains user features.")

(defconst site-user-feature-directory (concat user-emacs-directory "site-lisp/")
  "Directory which contains site user features.")

(defun user-feature-catch-byte-code-up (file)
  "Byte compile FILE if byte code file is not up to date."
  (let ((byte-code-file (byte-compile-dest-file file)))
    (unless (and (file-exists-p byte-code-file)
                 (file-newer-than-file-p byte-code-file file))
      (if byte-code-file
          (and (delete-file byte-code-file) (setq byte-code-file nil)))
      (and (byte-compile-file file)
           (setq byte-code-file (byte-compile-dest-file file))))
    (if (null byte-code-file)
        (error "Byte compile for `%s' failed" file))))

(defun user-feature-catch-loaddefs-up (directory &optional name)
  "Generate loaddfes for DIRECTORY if loaddefs is not up to date.
If optional argument NAME is a string, use it for file name
of loaddefs. Otherwise, basename of DIRECTORY suffixed with
-loaddefs.el is used."
  (let ((files (seq-filter (lambda (file) (not (file-directory-p file)))
                           (directory-files directory t "\\.el$")))
        (output-file
         (or name (let ((basename (file-name-nondirectory
                                   (replace-regexp-in-string
                                    "/$" "" directory))))
                    (concat directory
                            (if (string-suffix-p "/" directory) "" "/")
                            basename
                            "-loaddefs.el")))))
    (unless
        (or (null files)
            (and (file-exists-p output-file)
                 (seq-every-p (lambda (file)
                                (or (file-newer-than-file-p output-file file)
                                    (file-equal-p output-file file)))
                              files)))
      (if (file-exists-p output-file)
          (delete-file output-file))
      (make-directory-autoloads directory output-file))))

(mapc
 (lambda (raw)
   (let* ((path (expand-file-name raw))
          (subdirs (directory-directories-recursively path "")))
     (setq load-path `(,@subdirs ,path ,@load-path))

     (mapc (lambda (dir)
             (let ((loaddefs
                    (concat
                     dir
                     (if (string-match "/$" dir) "" "/")
                     (replace-regexp-in-string
                      "lisp" "user-feature"
                      (file-name-nondirectory
                       (replace-regexp-in-string "/$" "" path)))
                     (replace-regexp-in-string
                      "/" "-"
                      (let ((rest (replace-regexp-in-string
                                   (concat "^" (regexp-quote path)) "" dir)))
                        (if (string-empty-p rest)
                            "-"
                          (concat (if (string-prefix-p "/" rest)  "" "/")
                                  rest
                                  (if (string-suffix-p "/" rest)  "" "/")))))
                     "loaddefs.el")))
               (user-feature-catch-loaddefs-up dir loaddefs)
               (load loaddefs 'noerror 'nomessage)))
           `(,@subdirs ,path))

     (mapc #'user-feature-catch-byte-code-up
           (directory-files-recursively path "\\.el$"))))
 `(,user-feature-directory ,site-user-feature-directory))


(resolve user-feature)
