
;;;; init-abbrev.el


(premise init)
(premise custom)

(custom-set-variables
 '(abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
 '(save-abbrevs 'silently))

(with-eval-after-load 'abbrev
  (setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " Ab")

  (if (file-readable-p abbrev-file-name) (quietly-read-abbrev-file)))


(resolve init-abbrev)
