
;;;; init-quickrun.el


(premise init)
(premise bindings)
(premise inst-quickrun)

(eval-when-compile (require 'ruby-mode))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c q") #'quickrun))

(advice-add 'balance-mode-context :filter-return
            (lambda (ret)
              (if (eq major-mode 'quickrun--mode) #'balance-weight-mode ret)))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode 'quickrun--mode)
              (balance-mode-implement-keys
               (list (kbd "C-SPC")
                     (kbd "C-@")
                     (kbd "C-n")
                     (kbd "C-p")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-a")
                     (kbd "C-e")
                     (kbd "C-v")
                     (kbd "C-s")
                     (kbd "C-r")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l v"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC v") . ,(kbd "l v")))
               overriding-balance-weight-mode-map)
              (define-key overriding-balance-weight-mode-map (kbd "W")
                #'kill-ring-save))))


(resolve init-quickrun)
