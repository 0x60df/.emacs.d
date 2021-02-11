
;;;; inst-yester-theme.el


(premise init)
(premise user-feature)
(premise user-package)

(let* ((name "yester")
       (directory (concat user-package-directory name "-theme")))
  (add-to-list 'load-path directory)

  (user-feature-catch-byte-code-up (locate-file name load-path '(".el")))

  (add-to-list 'custom-theme-load-path directory))


(resolve inst-yester-theme)
