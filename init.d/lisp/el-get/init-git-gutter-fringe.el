
;;;; init-git-gutter-fringe.el



;;; base

(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(eval-after-load 'git-gutter
  '(custom-set-variables '(git-gutter:lighter " GG")))


;;; bindings

(global-set-key "\C-cv<" 'git-gutter:previous-hunk)
(global-set-key "\C-cv>" 'git-gutter:next-hunk)
(global-set-key "\C-cvp" 'git-gutter:previous-hunk)
(global-set-key "\C-cvn" 'git-gutter:next-hunk)
(global-set-key "\C-cvd" 'git-gutter:popup-hunk)
(global-set-key "\C-cvr" 'git-gutter:revert-hunk)
(global-set-key "\C-cvs" 'git-gutter:stage-hunk)
(global-set-key "\C-cv " 'git-gutter:mark-hunk)


;;; functions

(defun git-gutter:close-popup ()
  (interactive)
  (delete-window (git-gutter:popup-buffer-window)))
