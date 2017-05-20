
;;;; init-abbrev.el


(premise init)

(eval-after-load 'abbrev
  '(custom-set-variables '(abbrev-file-name
                           (concat user-emacs-directory "abbrev_defs"))
                         '(save-abbrevs 'silently)))

(if (file-readable-p abbrev-file-name) (quietly-read-abbrev-file))


(resolve init-abbrev)
