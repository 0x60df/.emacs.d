
;;;; bindings.el


(premise init)
(premise functions)
(premise frame)

(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "ESC M-S") 'event-apply-shift-modifier)
(define-key key-translation-map (kbd "ESC M-a") 'event-apply-alt-modifier)
(define-key key-translation-map (kbd "ESC M-c") 'event-apply-control-modifier)
(define-key key-translation-map (kbd "ESC M-h") 'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "ESC M-m") 'event-apply-meta-modifier)
(define-key key-translation-map (kbd "ESC M-s") 'event-apply-super-modifier)
(define-key key-translation-map (kbd "<home>") 'event-apply-alt-modifier)
(define-key key-translation-map (kbd "<end>") 'event-apply-meta-modifier)
(define-key key-translation-map (kbd "<prior>") 'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "<next>") 'event-apply-super-modifier)
(global-set-key "\C-ch" 'help-command)
(global-set-key "\C-cd" 'pwd)
(global-set-key "\C-csg" 'grep)
(global-set-key "\C-csl" 'lgrep)
(global-set-key "\C-csr" 'rgrep)
(global-set-key "\C-csf" 'grep-find)
(global-set-key "\C-cso" 'occur)
(global-set-key "\M-sg" 'grep)
(global-set-key "\M-sl" 'lgrep)
(global-set-key "\M-sr" 'rgrep)
(global-set-key "\M-sf" 'grep-find)
(global-set-key "\C-ct" 'eshell)
(global-set-key "\C-cw" 'eww)
(global-set-key "\C-cf" 'print-which-function)
(global-set-key "\M-Y" 'yank-pop-forwards)
(global-set-key (kbd "M-+") 'duplicate-and-comment-out)
(global-set-key (kbd "C-S-n") 'next-line-scroll-up)
(global-set-key (kbd "C-S-p") 'previous-line-scroll-down)
(global-set-key (kbd "C->") 'next-error)
(global-set-key (kbd "C-<") 'previous-error)
(global-set-key (kbd "H-f") #'search-forward-char)
(global-set-key (kbd "H-b") #'search-backward-char)
(global-set-key (kbd "C-:") 'split-window-below)
(global-set-key (kbd "C-M-:") 'split-window-right)
(global-set-key (kbd "C-*") 'delete-other-windows)
(global-set-key (kbd "C-M-*") 'delete-window)
(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-M-,")
                (lambda (arg) (interactive "p") (other-window (* arg -1))))
(global-set-key (kbd "C-c :") 'manipulate-window)
(global-set-key (kbd "C-;") 'make-frame)
(global-set-key (kbd "C-+") 'delete-frame)
(global-set-key (kbd "C-.") 'other-frame)
(global-set-key (kbd "C-M-.")
                (lambda (arg) (interactive "p") (other-frame (* arg -1))))
(global-set-key (kbd "C-c ;") 'manipulate-frame)
(global-set-key (kbd "s-;") 'toggle-frame-opacity)
(global-set-key (kbd "s-+") 'toggle-all-frames-opacity)
(global-set-key (kbd "H-; s") 'set-frame-alpha)
(global-set-key (kbd "H-+ s") 'set-all-frames-alpha)
(global-unset-key "\C-l")
(global-set-key "\C-l\C-l" 'recenter-top-bottom)
(global-set-key "\C-l\C-f" 'find-file-at-point)
(global-set-key "\C-l\C-c" 'save-buffers-kill-emacs)


(resolve bindings)
