
;;;; bindings.el


(premise init)
(premise simple)
(premise risky)
(premise window)
(premise frame)
(premise client)
(premise mode-line)

(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "ESC M-DEL") [?\C-h])

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

(defvar overriding-standard-key-map (make-sparse-keymap)
  "Overriding map for standard reserved keys.")

(defvar overriding-standard-key t
  "If non-nil, `overriding-standard-key-map' is activated.")

(defvar overriding-major-mode-key-map (make-sparse-keymap)
  "Overriding map for reserved keys for major mode.")

(defvar overriding-major-mode-key t
  "If non-nil, `overriding-major-mode-key-map' is activated.")

(defvar overriding-minor-mode-key-map (make-sparse-keymap)
  "Overriding map for reserved keys for minor mode.")

(defvar overriding-minor-mode-key t
  "If non-nil, `overriding-minor-mode-key-map' is activated.")

(defvar overriding-reserved-key-map-alist
  `((overriding-minor-mode-key . ,overriding-minor-mode-key-map)
    (overriding-major-mode-key . ,overriding-major-mode-key-map)
    (overriding-standard-key . ,overriding-standard-key-map))
  "Overriding map alist for reserved keys.")
(add-to-list 'emulation-mode-map-alists 'overriding-reserved-key-map-alist)

(defun overriding-map-for (key)
  "Return appropriate overriding keymap for KEY.

keymap will be selected as follows.
Standard researved key: `overriding-standard-key-map'.
Researved for major mode: `overriding-major-mode-key-map'.
Researved for minor mode: `overriding-minor-mode-key-map'.
If KEY is a key researved for user, return `global-map',
because no other keymap may override it."
  (or (if (< 0 (length key))
          (let ((k1 (aref key 0)))
            (cond ((and (characterp k1)
                        (= k1 ?\^C)
                        (< 1 (length key)))
                   (let ((k2 (aref key 1)))
                     (if (characterp k2)
                         (cond ((or (and (<= ?A k2) (<= k2 ?Z))
                                    (and (<= ?a k2) (<= k2 ?z)))
                                global-map)
                               ((or (and (<= ?\^@ k2) (<= k2 ?\^_))
                                    (= k2 ?\^?)
                                    (and (<= ?0 k2) (<= k2 ?9))
                                    (memql k2 '(?{ ?} ?< ?> ?: ?\;)))
                                overriding-major-mode-key-map)
                               ((or (and (<= ?! k2) (<= k2 ?/))
                                    (and (<= ?\[ k2) (<= k2 ?\`))
                                    (memql k2 '(?= ?? ?@ ?\| ?~)))
                                overriding-minor-mode-key-map)))))
                  ((and (memq k1 '(f5 f6 f7 f8 f9))) global-map))))
      overriding-standard-key-map))

(defun overriding-set-key (key command)
  "Give KEY a overriding binding as COMMAND.
Keymap is determined by `overriding-map-for'"
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set overriding key: " nil t)))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key (overriding-map-for key) key command))



(overriding-set-key (kbd "C-c h") #'help-command)
(overriding-set-key (kbd "C->") #'next-error)
(overriding-set-key (kbd "C-<") #'previous-error)
(overriding-set-key (kbd "C-c i d") #'pwd)
(overriding-set-key (kbd "C-c [ e") #'exit-recursive-edit)
(overriding-set-key (kbd "C-c [ a") #'abort-recursive-edit)

(overriding-set-key (kbd "C-c s g") #'grep)
(overriding-set-key (kbd "C-c s l") #'lgrep)
(overriding-set-key (kbd "C-c s r") #'rgrep)
(overriding-set-key (kbd "C-c s z") #'zrgrep)
(overriding-set-key (kbd "C-c s f") #'grep-find)
(overriding-set-key (kbd "C-c s o") #'occur)
(overriding-set-key (kbd "M-s g") #'grep)
(overriding-set-key (kbd "M-s l") #'lgrep)
(overriding-set-key (kbd "M-s r") #'rgrep)
(overriding-set-key (kbd "M-s f") #'grep-find)



(overriding-set-key (kbd "M-+") #'duplicate-and-comment)
(overriding-set-key (kbd "H-y") #'yank-pop-reverse)
(overriding-set-key (kbd "C-S-n") #'next-line-scroll-up)
(overriding-set-key (kbd "C-S-p") #'previous-line-scroll-down)
(overriding-set-key (kbd "H-f") #'search-forward-char-in-line)
(overriding-set-key (kbd "H-b") #'search-backward-char-in-line)
(overriding-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(overriding-set-key (kbd "C-:") #'split-window-above)
(overriding-set-key (kbd "C-M-:") #'split-window-right)
(overriding-set-key (kbd "C-*") #'delete-other-windows)
(overriding-set-key (kbd "C-M-*") #'delete-window)
(overriding-set-key (kbd "C-,") #'other-window)
(overriding-set-key (kbd "C-M-,") #'other-window-reverse)
(overriding-set-key (kbd "C-c :") #'manipulate-window)
(overriding-set-key (kbd "C-c ,") #'view-other-window)

(overriding-set-key (kbd "C-;") #'make-frame)
(overriding-set-key (kbd "C-+") #'delete-frame)
(overriding-set-key (kbd "C-.") #'other-frame)
(overriding-set-key (kbd "C-M-.") #'other-frame-reverse)
(overriding-set-key (kbd "C-c ;") #'manipulate-frame)
(overriding-set-key (kbd "C-c .") #'pick-frame)
(overriding-set-key (kbd "s-;") #'toggle-frame-opacity)
(overriding-set-key (kbd "s-+") #'toggle-all-frames-opacity)

(with-eval-after-load 'server
  (overriding-set-key (kbd "C-.") #'other-frame-on-selected-client)
  (overriding-set-key (kbd "C-M-.") #'other-frame-on-selected-client-reverse)
  (overriding-set-key (kbd "s-.") #'other-client-frame)
  (overriding-set-key (kbd "s-M-.") #'other-client-frame-reverse)
  (overriding-set-key (kbd "C-c .") #'pick-frame-on-selected-client)
  (overriding-set-key (kbd "s-c .") #'pick-typical-frame-of-each-client))

(overriding-set-key (kbd "C-c l m") #'mode-line-modes-toggle-shrinked)
(overriding-set-key (kbd "C-c l b")
                    #'mode-line-buffer-identification-toggle-shrinked)
(overriding-set-key (kbd "C-c l i")
                    #'mode-line-mule-info-toggle-showing-input-method)
(overriding-set-key (kbd "C-c l f") #'show-which-function)
(overriding-set-key (kbd "C-c i f") #'show-which-function)



(overriding-set-key (kbd "C-l C-l") #'recenter-top-bottom)
(overriding-set-key (kbd "C-l C-f") #'find-file-at-point)
(overriding-set-key (kbd "C-l C-c") #'save-buffers-kill-emacs)


(resolve bindings)
