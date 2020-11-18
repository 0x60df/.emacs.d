
;;;; inst-evil.el


(premise init)
(premise init-el-get)

(defun el-get-suppress-recipe-for-evil (return)
  "Advising `el-get-read-recipe' to suppress recipe."
  (if (eq (plist-get return :name) 'evil)
      (let* ((definition (copy-tree return))
             (commands (lambda (keyword)
                         (let ((recipe (plist-get definition keyword)))
                           (if (symbolp (car recipe)) (cadr recipe) recipe))))
             (build (funcall commands :build))
             (build/berkeley-unix (funcall commands :build/berkeley-unix))
             (build/darwin (funcall commands :build/darwin)))
        (if build (setcar build (delete "info" (car build))))
        (if build/berkeley-unix
            (setcar build/berkeley-unix
                    (delete "info" (car build/berkeley-unix))))
        (if build/darwin
            (setcar build/darwin (delete "info" (car build/darwin))))
        (plist-put (plist-put definition :features nil) :info nil))
    return))

(advice-add 'el-get-read-recipe :filter-return
            #'el-get-suppress-recipe-for-evil)

(el-get-bundle evil)

(advice-remove 'el-get-read-recipe #'el-get-suppress-recipe-for-evil)


(resolve inst-evil)
