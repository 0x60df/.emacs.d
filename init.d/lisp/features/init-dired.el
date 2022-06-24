
;;;; init-dired.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode #'dired-mode)
              (define-key overriding-balance-weight-mode-map
                (kbd "xx") (key-binding (kbd "x")))

              (define-key overriding-balance-weight-mode-map
                (kbd "S") (key-binding (kbd "s")))

              (balance-mode-implement-keys
               (list (kbd "C-v")
                     (kbd "C-s")
                     (kbd "C-l C-c")
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
                     (kbd "C-c l l")
                     (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1"))
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
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l"))
                 (,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1")))
               overriding-balance-weight-mode-map))))


(resolve init-dired)
