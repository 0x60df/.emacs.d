
;;;; init-helm.el


(premise init)
(premise custom)
(premise subr)
(premise mode-line)
(premise bindings)
(premise init-ido)
(premise inst-helm)

(eval-when-compile
  (require 'helm)
  (require 'helm-mode))

(declare-function helm-execute-persistent-action "helm")
(declare-function helm-select-action "helm")


;;; settings

(custom-set-variables
 '(helm-truncate-lines t))

(with-eval-after-load 'helm-mode
  (setq helm-completion-mode-string " H")

  (add-to-list 'helm-this-command-black-list 'execute-extended-command)
  (if (init-unit-p inst-smex)
      (add-to-list 'helm-this-command-black-list 'smex))

  (mapc (lambda (handler)
          (add-to-list 'helm-completing-read-handlers-alist handler))
        '((find-file . ido-read-file-name)
          (find-alternate-file . ido-read-file-name)
          (switch-to-buffer . ido-completing-read)
          (kill-buffer . ido-completing-read)
          (load-file . ido-read-file-name)
          (sdired-sort-by . ido-completing-read)))
  (with-eval-after-load 'dired
    (add-to-list 'helm-completing-read-handlers-alist
                  '(dired . ido-read-file-name)))
  (with-eval-after-load 'sdired
    (add-to-list 'helm-completing-read-handlers-alist
                 '(dired . ido-read-file-name)))

  (add-hook 'helm-major-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))



;;; mode-line

(push '(helm-mode . 10) mode-line-minor-mode-priority-alist)

(defun modify-helm-mode-line (&rest args)
  "Advising function to modify mode-line-format for helm."
  (when helm-mode-line-string
    (let ((head (cadr mode-line-format)))
      (setcdr head (cons 'mode-line-front-space (cdr head)))
      (setcar head ""))
    (setq mode-line-format (mode-line-format-auto-truncate mode-line-format))))

(with-eval-after-load 'helm
  (add-to-list 'mode-line-boundary-faces 'helm-candidate-number)
  (advice-add 'helm-display-mode-line :after #'modify-helm-mode-line))



;;; bindings

(custom-set-variables
 '(helm-command-prefix-key "C-q"))

(with-eval-after-load 'helm-global-bindings
  (overriding-set-key (kbd "C-q") helm-command-map)

  (define-key helm-command-map (kbd "C-q") #'quoted-insert)
  (define-key helm-command-map (kbd "C-m") #'helm-mini)
  (define-key helm-command-map (kbd "C-f") #'helm-find-files)
  (define-key helm-command-map (kbd "b") #'helm-buffers-list)
  (define-key helm-command-map (kbd "M-x") #'helm-M-x)
  (define-key helm-command-map (kbd "C-o") #'helm-occur)
  (define-key helm-command-map (kbd "C-r") #'helm-register)
  (define-key helm-command-map (kbd "C-b") #'helm-filtered-bookmarks)
  (define-key helm-command-map (kbd "C-x r b") #'helm-bookmarks)
  (define-key helm-command-map (kbd "C-y") #'helm-show-kill-ring)

  (dolist (key (list (kbd "C-q C-q")
                     (kbd "C-q C-m")
                     (kbd "C-q M-x")
                     (kbd "C-q C-b")
                     (kbd "C-q C-r")
                     (kbd "C-q C-y")
                     (kbd "C-q i")))
    (add-to-list 'balance-mode-key-list key))

  (add-to-list 'balance-mode-key-alias-alist `(,(kbd "q SPC i") . ,(kbd "q i")))

  (add-hook 'helm-cleanup-hook
            (lambda ()
              (add-hook-for-once 'post-command-hook
                                 #'balance-mode-update-keys))))

(with-eval-after-load 'helm
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") #'helm-select-action))



;;; start

(remove-hook 'emacs-startup-hook #'ido-mode)
(add-hook 'emacs-startup-hook #'helm-mode)


(resolve init-helm)
