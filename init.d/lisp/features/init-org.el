
;;;; init-org.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)

(eval-when-compile (require 'org))
(declare-function org-time-stamp "org")
(declare-function org-time-stamp-inactive "org")

(custom-set-variables
 '(org-use-speed-commands t)
 '(org-speed-commands-user '(("s" . save-buffer)
                             ("g" . ignore)))
 '(org-loop-over-headlines-in-active-region t)
 '(org-highlight-latex-and-related '(latex entities))
 '(org-fontify-done-headline nil))

(push '(org-indent-mode . 1) mode-line-minor-mode-priority-alist)

(defun org-hide-trailing-whitespace-for-export-dispatcher (buffer &rest args)
  "Advising `org-switch-to-buffer-other-window' to hide whitespace."
  (if (eq buffer (get-buffer "*Org Export Dispatcher*"))
      (with-current-buffer "*Org Export Dispatcher*"
        (setq show-trailing-whitespace nil))))

(advice-add 'org-switch-to-buffer-other-window
            :before #'org-hide-trailing-whitespace-for-export-dispatcher)

(defcustom org-template `((standard . ,(concat "# -*- coding:utf-8 -*-\n"
                                               "#+STARTUP: indent\n")))
  "Alist of template for org-mode file.
Template must be a string or function that returns a string."
  :type '(alist :key-type symbol)
  :group 'user)

(defun org-insert-template (tag)
  "Insert template defined in `org-template' and tagged by TAG.
When called interactively, `org-template' is used for
`completing-read'."
  (interactive (list (if (eq major-mode 'org-mode)
                         (intern
                          (completing-read "Template: " org-template nil t))
                       (message "Here is not org-mode"))))
  (if (eq major-mode 'org-mode)
      (let* ((template-source (cdr (assq tag org-template)))
             (template (cond ((stringp template-source) template-source)
                             ((functionp template-source)
                              (funcall template-source))
                             (t ""))))
        (unless (save-excursion
                  (goto-char 1)
                  (looking-at (regexp-quote template)))
          (if (= (point) 1)
              (insert template)
            (save-excursion
              (goto-char 1)
              (insert template)))))))

(with-eval-after-load 'org-indent
  (modify-minor-mode-lighter 'org-indent-mode " ind"))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook
            (lambda ()
              (if header-line-format
                  (setq header-line-format
                        (mode-line-format-auto-truncate
                         (list mode-line-front-space header-line-format)))))))

(with-eval-after-load 'org-src
  (add-hook 'org-src-mode-hook
            (lambda ()
              (if header-line-format
                  (setq header-line-format
                        (mode-line-format-auto-truncate
                         (list mode-line-front-space header-line-format))))))
  (add-to-list 'org-src-lang-modes '("toml" . conf-toml)))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'quick-input-method-mode))

(advice-add 'balance-mode-context :filter-return
            (lambda (ret)
              (if (eq major-mode 'org-agenda-mode) #'balance-weight-mode ret)))

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o t") #'org-insert-template))

(add-to-list 'balance-mode-key-list (kbd "C-c o a"))
(add-to-list 'balance-mode-key-list (kbd "C-c o c"))
(add-to-list 'balance-mode-key-list (kbd "C-c o t"))
(add-to-list 'balance-mode-key-list (kbd "C-c C-x C-a"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC o") . ,(kbd "c o")))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode 'org-agenda-mode)
              (balance-mode-implement-keys
               (list (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c .")
                     (kbd "C-c ,")
                     (kbd "C-c h v")
                     (kbd "C-c h f")
                     (kbd "C-c l m")
                     (kbd "C-c l n")
                     (kbd "C-c l b")
                     (kbd "C-c l v")
                     (kbd "C-c l i")
                     (kbd "C-c l f")
                     (kbd "C-c l c")
                     (kbd "C-c l l")
                     (kbd "C-l C-c")
                     (kbd "C-l C-k C-s")
                     (kbd "C-l C-k C-f")
                     (kbd "C-l C-k C-c")
                     (kbd "C-l C-k C-e")
                     (kbd "C-l C-k C-v")
                     (kbd "C-l C-k d")
                     (kbd "C-l C-k r SPC")
                     (kbd "C-l C-k r N")
                     (kbd "C-l C-k r b")
                     (kbd "C-l C-k r j")
                     (kbd "C-l C-k 1")
                     (kbd "C-l t")
                     (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC l") . ,(kbd "c l"))
                 (,(kbd "l SPC t") . ,(kbd "l t"))
                 (,(kbd "l k SPC d") . ,(kbd "l k d"))
                 (,(kbd "l k SPC r SPC") . ,(kbd "l k r SPC"))
                 (,(kbd "l k SPC r N") . ,(kbd "l k r N"))
                 (,(kbd "l k SPC r b") . ,(kbd "l k r b"))
                 (,(kbd "l k SPC r j") . ,(kbd "l k r j"))
                 (,(kbd "l k SPC 1") . ,(kbd "l k 1"))
                 (,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1")))
               overriding-balance-weight-mode-map))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-.") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c C-!") #'org-time-stamp-inactive))


(resolve init-org)
