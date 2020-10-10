
;;;; init-magit.el



;;; base

(premise init)
(premise inst-magit)

(global-set-key "\C-cvm" 'magit-status)
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace nil))))
      '(magit-mode-hook
        magit-popup-mode-hook))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)


(resolve init-magit)
