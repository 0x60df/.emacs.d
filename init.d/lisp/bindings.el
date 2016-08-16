
;;;; bindings.el


(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-ch" 'help-command)
(global-set-key "\C-cp" 'pwd)
(global-set-key "\C-cg " 'grep)
(global-set-key "\C-cgf" 'grep-find)
(global-set-key "\C-cs" 'eshell)
(global-set-key "\C-cw" 'eww)
(global-set-key "\M-Y" 'yank-pop-forwards)
(global-set-key (kbd "M-+") 'duplicate-and-comment-out)
(global-set-key (kbd "C-S-n") 'next-line-recenter)
(global-set-key (kbd "C-S-p") 'previous-line-recenter)
(global-set-key (kbd "C->") 'next-error)
(global-set-key (kbd "C-<") 'previous-error)
(global-set-key (kbd "C-:") 'split-window-below)
(global-set-key (kbd "C-s-:") 'split-window-right)
(global-set-key (kbd "C-*") 'delete-window)
(global-set-key (kbd "C-s-*") 'delete-other-windows)
(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-s-,") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c :") 'manipulate-window)
(global-set-key (kbd "C-;") 'make-frame)
(global-set-key (kbd "C-+") 'delete-frame)
(global-set-key (kbd "C-.") 'other-frame)
(global-set-key (kbd "C-s-.") (lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "C-c ;") 'manipulate-frame)
