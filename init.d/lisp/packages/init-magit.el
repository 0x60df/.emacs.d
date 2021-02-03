
;;;; init-magit.el


(premise init)
(premise custom)
(premise bindings)
(premise init-ediff)
(premise inst-magit)

(eval-when-compile (require 'magit-blame))

(declare-function git-commit-turn-on-flyspell "git-commit")
(declare-function magit-blame "magit-blame")

(defface magit-blame-margin-body
  '((t :inherit magit-blame-dimmed
       :weight normal
       :slant normal
       :underline nil
       :inverse-video nil
       :extend t))
  "Face for magit-blame-margin-body"
  :group 'user)

(custom-set-variables
 '(transient-highlight-mismatched-keys t))

(with-eval-after-load 'magit-blame
  (let ((margin-style (cdr (assq 'margin magit-blame-styles))))
    (setcdr (assq 'margin-body-face margin-style)
            '(magit-blame-margin-body))
    (setcdr (assq 'margin-format margin-style)
            '(" %s%f" " %C %a" " %H" ""))))

(defun magit-blame-set-highlight-overlay-priority (ov &rest args)
  "Advising `magit-blame--update-highlight-overlay' to set priority."
  (overlay-put ov 'priority -51))

(advice-add 'magit-blame--update-highlight-overlay
            :after #'magit-blame-set-highlight-overlay-priority)

(overriding-set-key (kbd "H-v") #'magit-status)
(overriding-set-key (kbd "C-c v m") #'magit-status)
(overriding-set-key (kbd "C-c v b") #'magit-blame)

(with-eval-after-load 'magit-mode
  (add-hook 'magit-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(with-eval-after-load 'magit-ediff
  (add-hook 'magit-ediff-quit-hook #'ediff-restore-departure-frame))


(resolve init-magit)
