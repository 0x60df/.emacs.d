
;;;; vc.el


(premise init)

(custom-set-variables '(vc-follow-symlinks t))

(defun abbreviate-vc-git-mode-line-string (string)
  "abbreviate vc-git-mode-line-string."
  (cond ((string-prefix-p "Git" string)
         (concat "G"
                 (substring (cond ((string-suffix-p "master" string)
                                   (concat
                                    (substring string 0 (- (length string) 6))
                                    "m/"))
                                  (t string))
                            3)))
        (t string)))

(advice-add 'vc-git-mode-line-string :filter-return
            'abbreviate-vc-git-mode-line-string)


(resolve vc)
