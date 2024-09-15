
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

  (advice-add 'balance-mode-context :filter-return
              (lambda (ret)
                (if (string-equal (buffer-name) "*git-gutter:diff*")
                    #'balance-weight-mode
                  ret)))

  (add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (string-equal (buffer-name) "*git-gutter:diff*")
              (balance-mode-implement-keys
               (list (kbd "C-n")
                     (kbd "C-p")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-a")
                     (kbd "C-e")
                     (kbd "C-v")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l n")
                     (kbd "C-l p")
                     (kbd "C-l v")
                     (kbd "C-l C-l n")
                     (kbd "C-l C-l p")
                     (kbd "C-l C-l f")
                     (kbd "C-l C-l b"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC n") . ,(kbd "l n"))
                 (,(kbd "l SPC p") . ,(kbd "l p"))
                 (,(kbd "l SPC v") . ,(kbd "l v"))
                 (,(kbd "l l SPC n") . ,(kbd "l l n"))
                 (,(kbd "l l SPC p") . ,(kbd "l l p"))
                 (,(kbd "l l SPC f") . ,(kbd "l l f"))
                 (,(kbd "l l SPC b") . ,(kbd "l l b")))
               overriding-balance-weight-mode-map))))

  (if (or balance-mode balance-weight-mode) (balance-mode-update-keys)))

(add-hook 'find-file-hook (lambda ()
                            (if (vc-backend (buffer-file-name))
                                (git-gutter-mode))))


(resolve init-git-gutter)
