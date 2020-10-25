
;;;; bindings.el


(premise init)
(premise simple)
(premise risky)
(premise window)
(premise frame)
(premise client)
(premise mode-line)

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
(let ((form (lambda (&optional terminal)
              (define-key local-function-key-map (kbd "<home>")
                #'event-apply-alt-modifier)
              (define-key local-function-key-map (kbd "<end>")
                #'event-apply-alt-modifier)
              (define-key local-function-key-map (kbd "<prior>")
                #'event-apply-hyper-modifier)
              (define-key local-function-key-map (kbd "<next>")
                #'event-apply-super-modifier))))
  (funcall form)
  (add-hook 'after-make-terminal-functions form))



(global-set-key (kbd "C-c h") #'help-command)
(global-set-key (kbd "C->") #'next-error)
(global-set-key (kbd "C-<") #'previous-error)
(global-set-key (kbd "C-c i d") #'pwd)
(global-set-key (kbd "C-c [ e") #'exit-recursive-edit)
(global-set-key (kbd "C-c [ a") #'abort-recursive-edit)

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
(global-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(global-set-key (kbd "C-:") #'split-window-above)
(global-set-key (kbd "C-M-:") #'split-window-right)
(global-set-key (kbd "C-*") #'delete-other-windows)
(global-set-key (kbd "C-M-*") #'delete-window)
(global-set-key (kbd "C-,") #'other-window)
(global-set-key (kbd "C-M-,") #'other-window-reverse)
(global-set-key (kbd "C-c :") #'manipulate-window)
(global-set-key (kbd "C-c ,") #'view-other-window)

(global-set-key (kbd "C-;") #'make-frame)
(global-set-key (kbd "C-+") #'delete-frame)
(global-set-key (kbd "C-.") #'other-frame)
(global-set-key (kbd "C-M-.") #'other-frame-reverse)
(global-set-key (kbd "C-c ;") #'manipulate-frame)
(global-set-key (kbd "C-c .") #'pick-frame)
(global-set-key (kbd "s-;") #'toggle-frame-opacity)
(global-set-key (kbd "s-+") #'toggle-all-frames-opacity)

(with-eval-after-load 'server
  (global-set-key (kbd "C-.") #'other-frame-on-selected-client)
  (global-set-key (kbd "C-M-.") #'other-frame-on-selected-client-reverse)
  (global-set-key (kbd "s-.") #'other-client-frame)
  (global-set-key (kbd "s-M-.") #'other-client-frame-reverse)
  (global-set-key (kbd "C-c .") #'pick-frame-on-selected-client)
  (global-set-key (kbd "s-c .") #'pick-typical-frame-of-each-client))

(global-set-key (kbd "C-c l m") #'mode-line-modes-toggle-shrinked)
(global-set-key (kbd "C-c l b")
                #'mode-line-buffer-identification-toggle-shrinked)
(global-set-key (kbd "C-c l i")
                #'mode-line-mule-info-toggle-showing-input-method)
(global-set-key (kbd "C-c l f") #'show-which-function)
(global-set-key (kbd "C-c i f") #'show-which-function)



(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l C-l") #'recenter-top-bottom)
(global-set-key (kbd "C-l C-f") #'find-file-at-point)
(global-set-key (kbd "C-l C-c") #'save-buffers-kill-emacs)


(resolve bindings)
