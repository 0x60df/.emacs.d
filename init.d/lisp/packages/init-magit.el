
;;;; init-magit.el


(premise init)
(premise bindings)
(premise inst-magit)

(declare-function git-commit-turn-on-flyspell "git-commit")

(overriding-set-key (kbd "C-c v m") #'magit-status)

(with-eval-after-load 'magit-mode
  (add-hook 'magit-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))


(resolve init-magit)
