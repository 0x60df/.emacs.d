
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
                         (list mode-line-front-space header-line-format)))))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'quick-input-method-mode))

(advice-add 'balance-mode-context :filter-return
            (lambda (ret)
              (if (eq major-mode 'org-agenda-mode) #'balance-weight-mode ret)))

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)

(add-to-list 'balance-mode-key-list (kbd "C-c o a"))
(add-to-list 'balance-mode-key-list (kbd "C-c o c"))
(add-to-list 'balance-mode-key-list (kbd "C-c C-x C-a"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC o") . ,(kbd "c o")))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode 'org-agenda-mode)
              (balance-mode-implement-keys
               (list (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c .")
                     (kbd "C-c ,"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               (list `(,(kbd "c SPC ;") . ,(kbd "c ;"))
                     `(,(kbd "c SPC :") . ,(kbd "c :"))
                     `(,(kbd "c SPC .") . ,(kbd "c ."))
                     `(,(kbd "c SPC ,") . ,(kbd "c ,")))
               overriding-balance-weight-mode-map))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-.") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c C-!") #'org-time-stamp-inactive))


(resolve init-org)
