
;;;; init-git-gutter.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise inst-git-gutter)

(declare-function git-gutter:popup-buffer-window "git-gutter")
(declare-function git-gutter:previous-hunk "git-gutter")
(declare-function git-gutter:next-hunk "git-gutter")
(declare-function git-gutter:popup-hunk "git-gutter")
(declare-function git-gutter:revert-hunk "git-gutter")
(declare-function git-gutter:stage-hunk "git-gutter")
(declare-function git-gutter:mark-hunk "git-gutter")

(declare-function git-gutter:close-popup load-file-name t t)

(custom-set-variables
 '(git-gutter:lighter " GGt"))

(push '(git-gutter-mode . 33) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'git-gutter

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

  (add-to-list 'balance-mode-key-list (kbd "C-c v q"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v <"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v >"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v p"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v n"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v d"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v r"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v s"))
  (add-to-list 'balance-mode-key-list (kbd "C-c v SPC"))

  (add-to-list 'balance-mode-key-alias-alist
  `(,(kbd "c SPC v") . ,(kbd "c v"))))

(add-hook 'find-file-hook (lambda ()
                            (if (vc-backend (buffer-file-name))
                                (git-gutter-mode))))


(resolve init-git-gutter)
