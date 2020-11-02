
;;;; init-git-gutter-fringe.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-git-gutter-fringe)

(declare-function git-gutter:popup-buffer-window "git-gutter")
(declare-function git-gutter:previous-hunk "git-gutter")
(declare-function git-gutter:next-hunk "git-gutter")
(declare-function git-gutter:popup-hunk "git-gutter")
(declare-function git-gutter:revert-hunk "git-gutter")
(declare-function git-gutter:stage-hunk "git-gutter")
(declare-function git-gutter:mark-hunk "git-gutter")

(custom-set-variables
 '(git-gutter:lighter " GG"))

(defun git-gutter:close-popup ()
  "Close git gutter popup window."
  (interactive)
  (delete-window (git-gutter:popup-buffer-window)))

(overriding-set-key (kbd "C-c v q") #'git-gutter:close-popup)
(overriding-set-key (kbd "C-c v <") #'git-gutter:previous-hunk)
(overriding-set-key (kbd "C-c v >") #'git-gutter:next-hunk)
(overriding-set-key (kbd "C-c v p") #'git-gutter:previous-hunk)
(overriding-set-key (kbd "C-c v n") #'git-gutter:next-hunk)
(overriding-set-key (kbd "C-c v d") #'git-gutter:popup-hunk)
(overriding-set-key (kbd "C-c v r") #'git-gutter:revert-hunk)
(overriding-set-key (kbd "C-c v s") #'git-gutter:stage-hunk)
(overriding-set-key (kbd "C-c v SPC") #'git-gutter:mark-hunk)

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'git-gutter-fringe)
            (global-git-gutter-mode)))


(resolve init-git-gutter-fringe)
