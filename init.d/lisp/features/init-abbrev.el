
;;;; init-abbrev.el


(premise init)
(premise custom)
(premise mode-line)

(custom-set-variables
 '(abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
 '(save-abbrevs 'silently))

(push '(abbrev-mode . 43) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'abbrev
  (setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " Abrv")

  (if (file-readable-p abbrev-file-name) (quietly-read-abbrev-file)))


(resolve init-abbrev)
