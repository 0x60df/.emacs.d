
;;;; init-helm.el



;;; base

(premise init)
(premise inst-helm)

(add-hook 'helm-mode-hook (lambda () (icomplete-mode -1)))
(setq helm-completion-mode-string " H")


;;; ido

(ido-mode -1)

(defadvice ido-completing-read (before override-prompt)
  (cond ((eq this-command 'switch-to-buffer)
         (setq prompt "Buffer: "))
        ((eq this-command 'kill-buffer)
         (setq prompt "Kill buffer: "))))

(defadvice ido-read-file-name (before override-prompt)
  (cond ((eq this-command 'dired)
         (setq prompt "Dired: "))
        ((eq this-command 'ruby-load-file)
         (setq prompt "Load Ruby file: "))))

(ad-activate 'ido-completing-read)
(ad-activate 'ido-read-file-name)

(eval-after-load 'helm-config
  '(progn
     (add-to-list 'helm-completing-read-handlers-alist
                  '(find-file . ido-read-file-name))
     (add-to-list 'helm-completing-read-handlers-alist
                  '(switch-to-buffer . ido-completing-read))
     (add-to-list 'helm-completing-read-handlers-alist
                  '(kill-buffer . ido-completing-read))
     (add-to-list 'helm-completing-read-handlers-alist
                  '(dired . ido-read-file-name))
     (add-to-list 'helm-completing-read-handlers-alist
                  '(find-alternate-file . ido-read-file-name))
     (add-to-list 'helm-completing-read-handlers-alist
                  '(ruby-load-file . ido-read-file-name))))


;;; bindings

(eval-after-load 'helm-config
  '(custom-set-variables '(helm-command-prefix-key "C-q")))
(define-key helm-command-map (kbd "C-q") #'quoted-insert)
(define-key helm-command-map (kbd "C-m") #'helm-mini)
(define-key helm-command-map (kbd "C-f") #'helm-find-files)
(define-key helm-command-map (kbd "b") #'helm-buffers-list)
(define-key helm-command-map (kbd "M-x") #'helm-M-x)
(define-key helm-command-map (kbd "C-o") #'helm-occur)
(define-key helm-command-map (kbd "C-r") #'helm-register)
(define-key helm-command-map (kbd "C-b") #'helm-bookmarks)
(define-key helm-command-map (kbd "C-y") #'helm-show-kill-ring)

(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-j") #'helm-select-action)


(resolve init-helm)
