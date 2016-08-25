
;;;; init-git-gutter-fringe.el



;;; base

(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setcar (cdr (assq 'git-gutter-mode minor-mode-alist)) " GG")


;;; bindings

(global-set-key "\C-cv<" 'git-gutter:previous-hunk)
(global-set-key "\C-cv>" 'git-gutter:next-hunk)
(global-set-key "\C-cvp" 'git-gutter:previous-hunk)
(global-set-key "\C-cvn" 'git-gutter:next-hunk)
(global-set-key "\C-cvd" 'git-gutter:popup-hunk)
(global-set-key "\C-cvr" 'git-gutter:revert-hunk)
(global-set-key "\C-cvs" 'git-gutter:stage-hunk)
(global-set-key "\C-cv " 'git-gutter:mark-hunk)
