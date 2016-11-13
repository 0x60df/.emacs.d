
;;;; init-abbrev.el


(custom-set-variables
 '(abbrev-file-name "~/.emacs.d/abbrev_defs")
 '(save-abbrevs 'silently))

(if (file-readable-p abbrev-file-name) (quietly-read-abbrev-file))
