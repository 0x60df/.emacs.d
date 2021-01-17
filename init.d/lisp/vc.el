
;;;; vc.el


(premise init)
(premise custom)

(custom-set-variables
 '(vc-follow-symlinks t))

(defun abbreviate-vc-git-mode-line-string (string)
  "Abbreviate vc-git-mode-line-string."
  (cond ((string-prefix-p "Git" string)
         (concat (apply #'propertize "G" (text-properties-at 0 string))
                 (substring (cond ((string-suffix-p "master" string)
                                   (concat
                                    (substring string 0 (- (length string) 6))
                                    (apply #'propertize "m/"
                                           (text-properties-at
                                            (- (length string) 1) string))))
                                  (t string))
                            3)))
        (t string)))

(advice-add 'vc-git-mode-line-string
            :filter-return #'abbreviate-vc-git-mode-line-string)


(resolve vc)
