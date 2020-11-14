
;;;; init-helm.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise init-icomplete)
(premise init-ido)
(premise init-dired)
(premise init-sdired)
(premise inst-helm)


;;; settings

(custom-set-variables
 '(helm-truncate-lines t))

(with-eval-after-load 'helm-mode
  (setq helm-completion-mode-string " H")

  (mapc (lambda (handler)
          (add-to-list 'helm-completing-read-handlers-alist handler))
        '((find-file . ido-read-file-name)
          (find-alternate-file . ido-read-file-name)
          (switch-to-buffer . ido-completing-read)
          (kill-buffer . ido-completing-read)
          (load-file . ido-read-file-name)
          (dired . ido-read-file-name)
          (sdired-sort-by . ido-completing-read))))



;;; patches

(defun modify-helm-mode-line (&rest args)
  "Advising function to modify mode-line-format for helm."
  (when helm-mode-line-string
    (let ((head (cadr mode-line-format)))
      (setcdr head (cons 'mode-line-front-space (cdr head)))
      (setcar head ""))
    (setq mode-line-format (mode-line-format-auto-truncate mode-line-format))))

(advice-add 'helm-display-mode-line :after #'modify-helm-mode-line)



;;; bindings

(custom-set-variables
 '(helm-command-prefix-key "C-q"))

(with-eval-after-load 'helm-config
  (overriding-set-key (kbd "C-q") helm-command-map)

  (define-key helm-command-map (kbd "C-q") #'quoted-insert)
  (define-key helm-command-map (kbd "C-m") #'helm-mini)
  (define-key helm-command-map (kbd "C-f") #'helm-find-files)
  (define-key helm-command-map (kbd "b") #'helm-buffers-list)
  (define-key helm-command-map (kbd "M-x") #'helm-M-x)
  (define-key helm-command-map (kbd "C-o") #'helm-occur)
  (define-key helm-command-map (kbd "C-r") #'helm-register)
  (define-key helm-command-map (kbd "C-b") #'helm-bookmarks)
  (define-key helm-command-map (kbd "C-y") #'helm-show-kill-ring))

(with-eval-after-load 'helm
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") #'helm-select-action))



;;; start

(remove-hook 'emacs-startup-hook #'ido-mode)
(add-hook 'emacs-startup-hook #'helm-mode)


(resolve init-helm)
