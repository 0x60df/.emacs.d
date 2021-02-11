
;;;; inst-helm-frames.el


(premise init)
(premise user-feature)
(premise user-package)

(let* ((name "helm-frames")
       (directory (concat user-package-directory name)))
  (add-to-list 'load-path directory)

  (let ((path (locate-file name load-path '(".el"))))
    (user-package-resolve-requires path)

    (user-feature-catch-byte-code-up path))

  (user-feature-catch-loaddefs-up directory)
  (load (concat directory "/" name "-loaddefs.el") 'noerror 'nomessage))


(resolve inst-helm-frames)
