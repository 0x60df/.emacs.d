
;;;; init-magit.el


(premise init)
(premise custom)
(premise bindings)
(premise init-ediff)
(premise inst-magit)

(declare-function git-commit-turn-on-flyspell "git-commit")

(custom-set-variables
 '(transient-highlight-mismatched-keys t))

(overriding-set-key (kbd "C-c v m") #'magit-status)

(with-eval-after-load 'magit-mode
  (add-hook 'magit-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(with-eval-after-load 'magit-ediff
  (add-hook 'magit-ediff-quit-hook #'ediff-restore-departure-frame))


(resolve init-magit)
