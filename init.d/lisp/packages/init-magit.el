
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

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode 'magit-status-mode)
              (let ((entry (lookup-key (current-local-map) (kbd "x"))))
                (when (and entry (not (numberp entry)))
                  (echo entry)
                    (define-key overriding-balance-weight-mode-map
                      (kbd "xx") entry)))
              (let ((entry (lookup-key (current-local-map) (kbd "c"))))
                (if (and entry (not (numberp entry)))
                    (define-key overriding-balance-weight-mode-map
                      (kbd "cc") entry)))
              (balance-mode-implement-keys
               (list (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1")
                     (kbd "C-l C-c")
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
                     (kbd "C-c l l"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1"))
                 (,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l")))
               overriding-balance-weight-mode-map))))

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
              (if (string-equal (buffer-name) "COMMIT_EDITMSG")
                  (prog1 nil
                    (run-with-timer 0.8 nil #'balance-mode-update-cursor-color))
                ret)))


(resolve init-magit)
