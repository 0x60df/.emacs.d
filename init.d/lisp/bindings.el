
;;;; bindings.el


(premise init)
(premise simple)
(premise window)
(premise frame)

(define-key key-translation-map [?\C-h] [?\C-?])

(define-key key-translation-map (kbd "ESC M-S") #'event-apply-shift-modifier)
(define-key key-translation-map (kbd "ESC M-a") #'event-apply-alt-modifier)
(define-key key-translation-map (kbd "ESC M-c") #'event-apply-control-modifier)
(define-key key-translation-map (kbd "ESC M-h") #'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "ESC M-m") #'event-apply-meta-modifier)
(define-key key-translation-map (kbd "ESC M-s") #'event-apply-super-modifier)

(global-unset-key (kbd "<home>"))
(global-unset-key (kbd "<end>"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))
(funcall
 (if (daemonp)
     (lambda (body) (add-hook 'after-make-terminal-functions body))
   #'funcall)
 (lambda (&optional terminal)
   (define-key local-function-key-map (kbd "<home>")
     #'event-apply-alt-modifier)
   (define-key local-function-key-map (kbd "<end>")
     #'event-apply-alt-modifier)
   (define-key local-function-key-map (kbd "<prior>")
     #'event-apply-hyper-modifier)
   (define-key local-function-key-map (kbd "<next>")
     #'event-apply-super-modifier)))

(global-set-key (kbd "C-c h") #'help-command)
(global-set-key (kbd "C->") #'next-error)
(global-set-key (kbd "C-<") #'previous-error)
(global-set-key (kbd "C-c d") #'pwd)

(global-set-key (kbd "C-c s g") #'grep)
(global-set-key (kbd "C-c s l") #'lgrep)
(global-set-key (kbd "C-c s r") #'rgrep)
(global-set-key (kbd "C-c s f") #'grep-find)
(global-set-key (kbd "C-c s o") #'occur)
(global-set-key (kbd "M-s g") #'grep)
(global-set-key (kbd "M-s l") #'lgrep)
(global-set-key (kbd "M-s r") #'rgrep)
(global-set-key (kbd "M-s f") #'grep-find)

(global-set-key (kbd "M-+") #'duplicate-and-comment)
(global-set-key (kbd "H-y") #'yank-pop-reverse)
(global-set-key (kbd "C-S-n") #'next-line-scroll-up)
(global-set-key (kbd "C-S-p") #'previous-line-scroll-down)
(global-set-key (kbd "H-f") #'search-forward-char-in-line)
(global-set-key (kbd "H-b") #'search-backward-char-in-line)

(global-set-key (kbd "C-:") #'split-window-below)
(global-set-key (kbd "C-M-:") #'split-window-right)
(global-set-key (kbd "C-*") #'delete-other-windows)
(global-set-key (kbd "C-M-*") #'delete-window)
(global-set-key (kbd "C-,") #'other-window)
(global-set-key (kbd "C-M-,") #'other-window-reverse)
(global-set-key (kbd "C-c :") #'manipulate-window)

(global-set-key (kbd "C-;") #'make-frame)
(global-set-key (kbd "C-+") #'delete-frame)
(global-set-key (kbd "C-.") #'other-frame)
(global-set-key (kbd "C-M-.") #'other-frame-reverse)
(global-set-key (kbd "C-c ;") #'manipulate-frame)
(global-set-key (kbd "C-c .") #'raise-other-frame)
(global-set-key (kbd "s-;") #'toggle-frame-opacity)
(global-set-key (kbd "s-+") #'toggle-all-frames-opacity)

(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l C-l") #'recenter-top-bottom)
(global-set-key (kbd "C-l C-f") #'find-file-at-point)
(global-set-key (kbd "C-l C-c") #'save-buffers-kill-emacs)


(resolve bindings)
