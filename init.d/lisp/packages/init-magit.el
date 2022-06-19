
;;;; init-magit.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise client)
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
 '(transient-highlight-mismatched-keys t)
 '(with-editor-mode-lighter " WtE"))

(push '(with-editor-mode . 34) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'magit-blame
  (let ((margin-style (cdr (assq 'margin magit-blame-styles))))
    (when margin-style
      (setcdr (assq 'margin-body-face margin-style)
              '(magit-blame-margin-body))
      (setcdr (assq 'margin-format margin-style)
              '(" %s%f" " %C %a" " %H" "")))))

(defun magit-blame-set-highlight-overlay-priority (ov &rest args)
  "Advising `magit-blame--update-highlight-overlay' to set priority."
  (overlay-put ov 'priority -51))

(advice-add 'magit-blame--update-highlight-overlay
            :after #'magit-blame-set-highlight-overlay-priority)

(overriding-set-key (kbd "H-v") #'magit-status)
(overriding-set-key (kbd "C-c v m") #'magit-status)
(overriding-set-key (kbd "C-c v c") #'magit-checkout)
(overriding-set-key (kbd "C-c v b") #'magit-blame)

(add-to-list 'balance-mode-key-list (kbd "C-c v m"))
(add-to-list 'balance-mode-key-list (kbd "C-c v c"))
(add-to-list 'balance-mode-key-list (kbd "C-c v b"))

(add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC v") . ,(kbd "c v")))

(with-eval-after-load 'magit-mode
  (add-hook 'magit-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil))))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd ",") #'other-window)
  (define-key magit-status-mode-map (kbd ".") #'other-frame-with-server))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(with-eval-after-load 'magit-ediff
  (add-hook 'magit-ediff-quit-hook #'ediff-restore-departure-frame))

(advice-add 'balance-mode-context :filter-return
            (lambda (ret)
              (unless (string-equal (buffer-name) "COMMIT_EDITMSG") ret)))


(resolve init-magit)
