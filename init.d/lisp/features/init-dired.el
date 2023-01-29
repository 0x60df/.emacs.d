
;;;; init-dired.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always))

(put 'dired-do-rename 'ido 'find-file)
(put 'dired-do-copy 'ido 'find-file)

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode #'dired-mode)
              (define-key overriding-balance-weight-mode-map
                (kbd "S") (key-binding (kbd "s")))

              (balance-mode-implement-keys
               (list (kbd "C-v")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-s")
                     (kbd "C-r")
                     (kbd "C-l C-c")
                     (kbd "C-l C-k C-f")
                     (kbd "C-l C-k C-c")
                     (kbd "C-l C-k d")
                     (kbd "C-l C-k 1")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l n")
                     (kbd "C-l p")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l v")
                     (kbd "C-c s g")
                     (kbd "C-c s l")
                     (kbd "C-c s r")
                     (kbd "C-c s z")
                     (kbd "C-c s f")
                     (kbd "C-c s o")
                     (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c ,")
                     (kbd "C-c .")
                     (kbd "C-c n")
                     (kbd "C-c h v")
                     (kbd "C-c h f")
                     (kbd "C-c i d")
                     (kbd "C-c i f")
                     (kbd "C-c l m")
                     (kbd "C-c l n")
                     (kbd "C-c l b")
                     (kbd "C-c l v")
                     (kbd "C-c l i")
                     (kbd "C-c l f")
                     (kbd "C-c l c")
                     (kbd "C-c l l"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "c SPC s g") . ,(kbd "c s g"))
                 (,(kbd "c SPC s l") . ,(kbd "c s l"))
                 (,(kbd "c SPC s r") . ,(kbd "c s r"))
                 (,(kbd "c SPC s z") . ,(kbd "c s z"))
                 (,(kbd "c SPC s f") . ,(kbd "c s f"))
                 (,(kbd "c SPC s o") . ,(kbd "c s o"))
                 (,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC n") . ,(kbd "c n"))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l"))
                 (,(kbd "l k SPC d") . ,(kbd "l k d"))
                 (,(kbd "l k SPC 1") . ,(kbd "l k 1"))
                 (,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC n") . ,(kbd "l n"))
                 (,(kbd "l SPC p") . ,(kbd "l p"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC v") . ,(kbd "l v")))
               overriding-balance-weight-mode-map))))

(advice-add 'dired-do-flagged-delete :after
            (lambda (&optional _)
              (if (and balance-weight-mode
                       (string-equal (current-message)
                                     "(No deletions requested)"))
                  (message (concat "(No deletions requested) "
                                   "--- transiently waiting for killing emacs"))
                  (let ((key (read-key)))
                    (if (eql key ?c)
                        (save-buffers-kill-terminal)
                      (setq unread-command-events
                            (cons key unread-command-events)))))))


(resolve init-dired)
