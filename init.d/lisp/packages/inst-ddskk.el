
;;;; inst-ddskk.el


(premise init)
(premise init-el-get)

(defun el-get-suppress-recipe-for-ddskk (return)
  "Advising `el-get-read-recipe' to suppress recipe."
  (if (eq (plist-get return :name) 'ddskk)
      (plist-put (copy-tree return) :features nil)
    return))

(advice-add 'el-get-read-recipe
            :filter-return #'el-get-suppress-recipe-for-ddskk)

(el-get-bundle ddskk)

(advice-remove 'el-get-read-recipe #'el-get-suppress-recipe-for-ddskk)


(resolve inst-ddskk)
