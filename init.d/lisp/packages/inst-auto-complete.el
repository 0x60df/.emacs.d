
;;;; inst-auto-complete.el


(premise init)
(premise init-el-get)

(defun el-get-suppress-recipe-for-auto-complete (return)
  "Advising `el-get-read-recipe' to suppress recipe."
  (if (eq (plist-get return :name) 'auto-complete)
      (let ((new-return (copy-tree return)))
        (plist-put new-return :features nil)
        (plist-put new-return :post-init nil))
    return))

(advice-add 'el-get-read-recipe
            :filter-return #'el-get-suppress-recipe-for-auto-complete)

(el-get-bundle auto-complete)

(advice-remove 'el-get-read-recipe #'el-get-suppress-recipe-for-auto-complete)


(resolve inst-auto-complete)
