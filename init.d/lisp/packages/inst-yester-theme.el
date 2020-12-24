
;;;; inst-yester-theme.el


(premise init)

(let* ((name "yester")
       (directory (concat user-emacs-directory "package/" name "-theme")))
  (add-to-list 'load-path directory)

  (add-to-list 'custom-theme-load-path directory))


(resolve inst-yester-theme)
