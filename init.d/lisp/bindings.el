;;; -*- lexical-binding: t -*-
;;;; bindings.el


(premise init)
(premise subr)
(premise simple)
(premise risky)
(premise window)
(premise frame)
(premise client)
(premise mode-line)

(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map (kbd "ESC M-DEL") [?\C-h])
(custom-set-variables
 '(help-char ?\^z))

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
(let ((form (lambda (&optional _)
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



(overriding-set-key (kbd "C-z") #'help-command)
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



(overriding-set-key (kbd "C-l C-l C-l") #'recenter-top-bottom)
(overriding-set-key (kbd "C-l C-f") #'find-file-at-point)
(overriding-set-key (kbd "C-l C-v") #'revert-buffer)
(overriding-set-key (kbd "C-l C-c") #'save-buffers-kill-emacs)
(overriding-set-key (kbd "C-l C-k") ctl-x-map)
(overriding-set-key (kbd "C-l -") #'auto-overwrite-mode)
(overriding-set-key (kbd "C-l a") (kbd "M-<"))
(overriding-set-key (kbd "C-l e") (kbd "M->"))
(overriding-set-key (kbd "C-l C-a") (kbd "M-<"))
(overriding-set-key (kbd "C-l C-e") (kbd "M->"))
(overriding-set-key (kbd "C-l n") #'next-line-scroll-up)
(overriding-set-key (kbd "C-l p") #'previous-line-scroll-down)
(overriding-set-key (kbd "C-l f") #'emulate-forward-word-consecutively)
(overriding-set-key (kbd "C-l b") #'emulate-backward-word-consecutively)
(overriding-set-key (kbd "C-l v") #'emulate-scroll-down-command)
(overriding-set-key (kbd "C-l C-l n") #'emulate-scroll-up-command)
(overriding-set-key (kbd "C-l C-l p") #'emulate-scroll-down-command)
(overriding-set-key (kbd "C-l C-l f") #'emulate-forward-sexp-consecutively)
(overriding-set-key (kbd "C-l C-l b") #'emulate-backward-sexp-consecutively)



(overriding-set-key (kbd "H-1") (key-binding (kbd "M-!")))
(overriding-set-key (kbd "H-4") (key-binding (kbd "M-$")))
(overriding-set-key (kbd "H-5") (key-binding (kbd "M-%")))
(overriding-set-key (kbd "C-H-5") (key-binding (kbd "C-M-%")))
(overriding-set-key (kbd "H-6") (key-binding (kbd "M-&")))
(overriding-set-key (kbd "H-7") (key-binding (kbd "M-'")))
(overriding-set-key (kbd "H-8") (key-binding (kbd "M-(")))
(overriding-set-key (kbd "H-9") (key-binding (kbd "M-)")))

(overriding-set-key (kbd "s-1") "!")
(overriding-set-key (kbd "s-2") "\"")
(overriding-set-key (kbd "s-3") "#")
(overriding-set-key (kbd "s-4") "$")
(overriding-set-key (kbd "s-5") "%")
(overriding-set-key (kbd "s-6") "&")
(overriding-set-key (kbd "s-7") "'")
(overriding-set-key (kbd "s-8") "(")
(overriding-set-key (kbd "s-9") ")")



(overriding-set-key (kbd "M-+") #'duplicate-and-comment)
(overriding-set-key (kbd "C-M-;") #'comment-switch)
(overriding-set-key (kbd "H-;") #'comment-switch)
(overriding-set-key (kbd "C-M-y") #'yank-pop-reverse)
(overriding-set-key (kbd "H-y") #'yank-pop-reverse)
(overriding-set-key (kbd "C-S-n") #'next-line-scroll-up)
(overriding-set-key (kbd "C-S-p") #'previous-line-scroll-down)
(overriding-set-key (kbd "M-g M-f") #'search-forward-char-in-line-consecutively)
(overriding-set-key (kbd "H-f") #'search-forward-char-in-line-consecutively)
(overriding-set-key (kbd "M-g M-b")
                    #'search-backward-char-in-line-consecutively)
(overriding-set-key (kbd "H-b") #'search-backward-char-in-line-consecutively)
(overriding-set-key (kbd "C-c r y") #'risky-yes-or-no-p-mode)

(overriding-set-key (kbd "C-:") #'split-window-above)
(overriding-set-key (kbd "C-M-:") #'split-window-right)
(overriding-set-key (kbd "C-M-*") #'delete-window)
(overriding-set-key (kbd "C-M-^") #'delete-window)
(overriding-set-key (kbd "C-*") #'delete-other-windows)
(overriding-set-key (kbd "C-,") #'other-window)
(overriding-set-key (kbd "C-^") #'split-window-above-or-other-window)
(overriding-set-key (kbd "C-M-,") #'other-window-reverse)
(overriding-set-key (kbd "C-c :") #'manipulate-window)
(overriding-set-key (kbd "C-c ,") #'view-other-window)

(overriding-set-key (kbd "C-;") #'make-frame)
(overriding-set-key (kbd "C-+") #'delete-frame)
(overriding-set-key (kbd "C-.") #'other-frame)
(overriding-set-key (kbd "C-M-.") #'other-frame-reverse)
(overriding-set-key (kbd "C-c ;") #'manipulate-frame)
(overriding-set-key (kbd "C-c .") #'pick-frame)
(overriding-set-key (kbd "C-l ;") #'cycle-frame)
(overriding-set-key (kbd "s-;") #'toggle-frame-opacity)
(overriding-set-key (kbd "s-+") #'toggle-all-frames-opacity)

(with-eval-after-load 'server
  (overriding-set-key (kbd "C-.") #'other-frame-with-server)
  (overriding-set-key (kbd "C-M-.") #'other-frame-with-server-reverse)
  (overriding-set-key (kbd "C-l .") #'other-client-frame-with-server)
  (overriding-set-key (kbd "s-.") #'other-client-frame-with-server)
  (overriding-set-key (kbd "C-l M-.") #'other-client-frame-with-server-reverse)
  (overriding-set-key (kbd "s-M-.") #'other-client-frame-with-server-reverse)
  (overriding-set-key (kbd "C-c .") #'pick-frame-with-server)
  (overriding-set-key (kbd "C-l C-.")
                      #'pick-typical-frame-of-each-client-with-server)
  (overriding-set-key (kbd "s-c .")
                      #'pick-typical-frame-of-each-client-with-server)
  (overriding-set-key (kbd "C-c M-.") #'summon-frame))

(overriding-set-key (kbd "C-c l m") #'mode-line-mode-name-shrink-mode)
(overriding-set-key (kbd "C-c l n") #'mode-line-minor-mode-shrink-mode)
(overriding-set-key (kbd "C-c l b")
                    #'mode-line-buffer-identification-shrink-mode)
(overriding-set-key (kbd "C-c l v")
                    #'mode-line-vc-mode-shrink-mode)
(overriding-set-key (kbd "C-c l i")
                    #'mode-line-mule-info-showing-input-method-mode)
(overriding-set-key (kbd "C-c l f") #'show-which-function)
(overriding-set-key (kbd "C-c l c") #'mode-line-show-truncated)
(overriding-set-key (kbd "C-c l l") #'mode-line-auto-show-truncated-mode)
(overriding-set-key (kbd "C-c i f") #'show-which-function)



(defvar balance-mode-key-list
  (list (kbd "C-w")
        (kbd "C-e")
        (kbd "C-r")
        (kbd "C-t")
        (kbd "C-y")
        (kbd "C-u")
        (kbd "C-q")
        (kbd "C-p")
        (kbd "C-a")
        (kbd "C-s")
        (kbd "C-d")
        (kbd "C-f")
        (kbd "C-g")
        (kbd "C-j")
        (kbd "C-k")
        (kbd "C-;")
        (kbd "C-:")
        (kbd "C-M-:")
        (kbd "C-+")
        (kbd "C-*")
        (kbd "C-M-*")
        (kbd "C-z")
        (kbd "C-v")
        (kbd "C-b")
        (kbd "C-n")
        (kbd "C-m")
        (kbd "C-,")
        (kbd "C-.")
        (kbd "C-/")
        (kbd "C-\\")
        (kbd "C-<")
        (kbd "C->")
        (kbd "C-@")
        (kbd "C-SPC")
        ;; (kbd "C-S-n")
        ;; (kbd "C-S-p")
        (kbd "C-l C-c")
        (kbd "C-l C-l C-l")
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
        (kbd "C-l -")
        (kbd "C-l a")
        (kbd "C-l e")
        (kbd "C-l n")
        (kbd "C-l p")
        (kbd "C-l f")
        (kbd "C-l b")
        (kbd "C-l v")
        (kbd "C-l C-l n")
        (kbd "C-l C-l p")
        (kbd "C-l C-l f")
        (kbd "C-l C-l b")
        (kbd "C-l ;")
        (kbd "C-c C-c")
        (kbd "C-c C-k")
        (kbd "C-x C-s")
        (kbd "C-x C-f")
        (kbd "C-x C-c")
        (kbd "C-x C-e")
        (kbd "C-x C-v")
        (kbd "C-x C-x")
        (kbd "C-x d")
        (kbd "C-x b")
        (kbd "C-x k")
        (kbd "C-x h")
        (kbd "C-x r SPC")
        (kbd "C-x r N")
        (kbd "C-x r b")
        (kbd "C-x r j")
        (kbd "C-x [")
        (kbd "C-x ]")
        (kbd "C-x 1")
        (kbd "C-x SPC")
        (kbd "C-c C-c")
        (kbd "C-c ;")
        (kbd "C-c :")
        (kbd "C-c ,")
        (kbd "C-c .")
        (kbd "C-c h")
        (kbd "C-c i d")
        (kbd "C-c i f")
        (kbd "C-c l m")
        (kbd "C-c l n")
        (kbd "C-c l b")
        (kbd "C-c l v")
        (kbd "C-c l i")
        (kbd "C-c l f")
        (kbd "C-c l c")
        (kbd "C-c l l")
        (kbd "C-c s g")
        (kbd "C-c s l")
        (kbd "C-c s r")
        (kbd "C-c s z")
        (kbd "C-c s f")
        (kbd "C-c s o"))
  "Key list which are implemented when `balance-mode' is enabled.")

(defvar balance-mode-key-alias-alist
  `((,(kbd "c SPC ;") . ,(kbd "c ;"))
    (,(kbd "c SPC :") . ,(kbd "c :"))
    (,(kbd "c SPC ,") . ,(kbd "c ,"))
    (,(kbd "c SPC .") . ,(kbd "c ."))
    (,(kbd "c SPC h") . ,(kbd "c h"))
    (,(kbd "x SPC d") . ,(kbd "x d"))
    (,(kbd "x SPC b") . ,(kbd "x b"))
    (,(kbd "x SPC k") . ,(kbd "x k"))
    (,(kbd "x SPC h") . ,(kbd "x h"))
    (,(kbd "x SPC r") . ,(kbd "x r"))
    (,(kbd "x SPC [") . ,(kbd "x ["))
    (,(kbd "x SPC ]") . ,(kbd "x ]"))
    (,(kbd "x SPC 1") . ,(kbd "x 1"))
    (,(kbd "l SPC -") . ,(kbd "l -"))
    (,(kbd "l SPC a") . ,(kbd "l a"))
    (,(kbd "l SPC e") . ,(kbd "l e"))
    (,(kbd "l SPC n") . ,(kbd "l n"))
    (,(kbd "l SPC p") . ,(kbd "l p"))
    (,(kbd "l SPC f") . ,(kbd "l f"))
    (,(kbd "l SPC b") . ,(kbd "l b"))
    (,(kbd "l SPC v") . ,(kbd "l v"))
    (,(kbd "l l SPC n") . ,(kbd "l l n"))
    (,(kbd "l l SPC p") . ,(kbd "l l p"))
    (,(kbd "l l SPC f") . ,(kbd "l l f"))
    (,(kbd "l l SPC b") . ,(kbd "l l b"))
    (,(kbd "l SPC ;") . ,(kbd "l ;"))
    (,(kbd "l k SPC d") . ,(kbd "l k d"))
    (,(kbd "l k SPC r SPC") . ,(kbd "l k r SPC"))
    (,(kbd "l k SPC r N") . ,(kbd "l k r N"))
    (,(kbd "l k SPC r b") . ,(kbd "l k r b"))
    (,(kbd "l k SPC r j") . ,(kbd "l k r j"))
    (,(kbd "l k SPC 1") . ,(kbd "l k 1"))
    (,(kbd "c SPC i") . ,(kbd "c i"))
    (,(kbd "c SPC l") . ,(kbd "c l"))
    (,(kbd "c SPC s") . ,(kbd "c s"))
    (,(kbd "x c") . ,(kbd "C")))
  "Key alias list which are defined when `balance-mode' is enabled.")

(defvar-local overriding-balance-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") (kbd "DEL"))
    (define-key map (kbd "i") (lambda ()
                                (interactive)
                                (balance-mode 0)
                                (if (eq (balance-mode-context)
                                        'balance-weight-mode)
                                    (balance-weight-mode))))
    (define-key map (kbd "[") (lambda ()
                                (interactive)
                                (balance-mode 0)
                                (if (eq (balance-mode-context)
                                        'balance-weight-mode)
                                    (balance-weight-mode))))
    (define-key map (kbd "R") #'replace-char)
    (define-key map (kbd "A") (kbd "ei"))
    (define-key map (kbd "Dd") #'kill-whole-line)
    (define-key map (kbd "Dw") #'kill-word)
    (define-key map (kbd "Ds") #'kill-sexp)
    (define-key map (kbd "De") #'kill-line)
    (define-key map (kbd "Da") (lambda ()
                                 (interactive)
                                 (kill-line 0)))
    (define-key map (kbd "Df") (lambda ()
                                 (interactive)
                                 (kill-region (point)
                                              (save-excursion
                                                (call-interactively
                                                 #'search-forward-char-in-line)
                                                (if (eobp)
                                                    (point)
                                                  (1+ (point)))))))
    (define-key map (kbd "W") #'kill-ring-save)
    (define-key map (kbd "H") #'help-command)
    (define-key map (kbd "lF") #'find-file-at-point)
    (define-key map (kbd "lV") #'revert-buffer)
    (define-key map (kbd "S-SPC") (lambda (arg)
                                    (interactive "p")
                                    (insert-char ?\s arg)))
    (define-key map (kbd "ESC M-SPC") #'global-balance-mode)
    map)
  "Overriding keymap for `balance-mode'.")

(defvar balance-weight-mode-key-list
  (list (kbd "C-;")
        (kbd "C-:")
        (kbd "C-M-:")
        (kbd "C-+")
        (kbd "C-*")
        (kbd "C-M-*")
        (kbd "C-,")
        (kbd "C-.")
        (kbd "C-<")
        (kbd "C->"))
  "Key list for `complementray-balance-mode'.")

(defvar balance-weight-mode-key-alias-alist nil
  "Key alias list for `balance-weight-mode'.")

(defvar-local overriding-balance-weight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") (lambda () (interactive) (balance-weight-mode 0)))
    (define-key map (kbd "[") (lambda () (interactive) (balance-weight-mode 0)))
    (define-key map (kbd "ESC M-SPC") (lambda ()
                                        (interactive)
                                        (balance-weight-mode 0)
                                        (balance-mode)))
    map)
  "Overriding keymap for `balance-weight-mode'.")

(defun balance-mode-digit-1 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-1") nil)))
(defun balance-mode-digit-2 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-2") nil)))
(defun balance-mode-digit-3 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-3") nil)))
(defun balance-mode-digit-4 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-4") nil)))
(defun balance-mode-digit-5 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-5") nil)))
(defun balance-mode-digit-6 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-6") nil)))
(defun balance-mode-digit-7 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-7") nil)))
(defun balance-mode-digit-8 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-8") nil)))
(defun balance-mode-digit-9 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-9") nil)))
(defun balance-mode-digit-0 ()
  "Emulate digit-argument."
  (interactive)
  (setq unread-command-events (append (kbd "C-0") nil)))

(dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  (dolist (map-variable '(overriding-balance-mode-map
                          overriding-balance-weight-mode-map))
    (define-key (default-value map-variable) key
                (intern (format "balance-mode-digit-%s" key)))))

(defvar balance-mode-update-keys-hook nil
  "Hook run at the last of `balance-mode-update-keys'")
(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (let ((indent-command (key-binding (kbd "C-M-\\") t)))
              (when (and indent-command (not (numberp indent-command)))
                (define-key overriding-balance-mode-map
                  (kbd "I") indent-command)
                (define-key overriding-balance-mode-map
                  (kbd "_") indent-command)))
            (let ((save-command (key-binding (kbd "C-x C-s") t)))
              (when (and save-command (not (numberp save-command)))
                (define-key overriding-balance-mode-map
                            (kbd "S") save-command)
                (define-key overriding-balance-mode-map
                            (kbd "-") save-command)))
            (let ((forward-command (key-binding (kbd "C-M-f") t)))
              (if (and forward-command (not (numberp forward-command)))
                  (define-key overriding-balance-mode-map
                              (kbd "F") forward-command)))
            (let ((backward-command (key-binding (kbd "C-M-b") t)))
              (if (and backward-command (not (numberp backward-command)))
                  (define-key overriding-balance-mode-map
                              (kbd "B") backward-command)))
            (let ((forward-command (key-binding (kbd "C-M-n") t)))
              (if (and forward-command (not (numberp forward-command)))
                  (define-key overriding-balance-mode-map
                              (kbd "N") forward-command)))
            (let ((backward-command (key-binding (kbd "C-M-p") t)))
              (if (and backward-command (not (numberp backward-command)))
                  (define-key overriding-balance-mode-map
                              (kbd "P") backward-command)))))

(defvar overriding-global-balance-mode-map (let ((map (make-sparse-keymap)))
                                             (define-key map (kbd "ESC M-SPC")
                                               (lambda ()
                                                 (interactive)
                                                 (if (eq (balance-mode-context)
                                                         'balance-weight-mode)
                                                     (balance-weight-mode)
                                                   (balance-mode))))
                                             map)
  "Overriding keymap for `global-balance-mode'.")

(defvar-local balance-mode-map-alist
  `((balance-mode . ,(default-value 'overriding-balance-mode-map))
    (balance-weight-mode . ,(default-value 'overriding-balance-weight-mode-map))
    (global-balance-mode . ,overriding-global-balance-mode-map))
  "Map alist for `balance-mode' added to `emulation-mode-map-alists'.")
(add-to-list 'emulation-mode-map-alists 'balance-mode-map-alist)

(defun balance-mode-context ()
  "Return mode function symbol suit with current context.
If there are no suitable `balance-mode' function, return nil."
  (cond ((minibufferp nil))
        ((or (derived-mode-p 'special-mode)
             (eq (get major-mode 'mode-class) 'special))
         'balance-weight-mode)
        (t 'balance-mode)))

(defun balance-mode-implement-keys (key-list keymap)
  "Implement KEY-LIST to KEYMAP according to `balance-mode' rule."
  (let ((balance-key-command-list
           (mapcar
            (lambda (key-sequence)
              (cons (let* ((key-sequence-vector (vconcat key-sequence) )
                           (control-sequence
                            (if (not (zerop (length key-sequence-vector)))
                                (memq 'control (event-modifiers
                                                (aref key-sequence-vector 0)))))
                           (key-list
                            (mapcan
                             (lambda (key)
                               (let ((modifiers (event-modifiers key))
                                     (basic-type (event-basic-type key)))
                                 (if (memq 'control modifiers)
                                     (let ((event
                                            (event-convert-list
                                             (append (remq 'control modifiers)
                                                     (list basic-type)))))
                                       (prog1 (if control-sequence
                                                  (list event)
                                                (list ?\s event))
                                         (setq control-sequence t)))
                                   (let ((event
                                          (event-convert-list
                                           (append modifiers
                                                   (list basic-type)))))
                                     (prog1 (if control-sequence
                                                (list ?\s event)
                                              (list event))
                                       (setq control-sequence nil))))))
                             key-sequence-vector)))
                      (vconcat key-list))
                    (key-binding key-sequence t)))
            key-list)))
      (dolist (key-command balance-key-command-list)
        (define-key keymap (car key-command) (cdr key-command)))))

(defun balance-mode-alias-keys (key-alias-alist keymap)
  "Define KEY-ALIAS-ALIST in KEYMAP.."
  (dolist (assoc key-alias-alist)
    (let ((entry (lookup-key keymap (car assoc))))
      (if (and entry (not (numberp entry)))
          (define-key keymap (cdr assoc) entry)))))

(defun balance-mode-clean-up-keys ()
  "Clean up keys of `balance-mode' on current buffer."
  (setq overriding-balance-mode-map
        (copy-keymap (default-value 'overriding-balance-mode-map)))
  (setq overriding-balance-weight-mode-map
        (copy-keymap (default-value 'overriding-balance-weight-mode-map)))
  (setq balance-mode-map-alist
        `((balance-mode . ,overriding-balance-mode-map)
          (balance-weight-mode . ,overriding-balance-weight-mode-map)
          (global-balance-mode . ,overriding-global-balance-mode-map))))

(defun balance-mode-update-keys ()
  "Update keys of `balance-mode'."
  (interactive)
  (balance-mode-clean-up-keys)
  (balance-mode-implement-keys balance-mode-key-list
                               overriding-balance-mode-map)
  (balance-mode-alias-keys balance-mode-key-alias-alist
                           overriding-balance-mode-map)
  (balance-mode-implement-keys balance-weight-mode-key-list
                               overriding-balance-weight-mode-map)
  (balance-mode-alias-keys balance-weight-mode-key-alias-alist
                           overriding-balance-weight-mode-map)
  (run-hooks 'balance-mode-update-keys-hook))

(defconst balance-mode-lighter-string " B" "Lighter string for `balance-mode'.")

(define-minor-mode balance-mode
  "Minor mode providing key bindings without control key."
  :group 'user
  :lighter (:propertize balance-mode-lighter-string face mode-line-warning)
  (if (and balance-mode (not (local-variable-p 'overriding-balance-mode-map)))
      (balance-mode-update-keys)))

(define-minor-mode balance-weight-mode
  "Complementary minor mode for `balance-mode'."
  :group 'user
  :lighter (:propertize balance-mode-lighter-string face mode-line-emphasis)
  (if (and balance-weight-mode
           (not (local-variable-p 'overriding-balance-weight-mode-map)))
      (balance-mode-update-keys)))

(defvar global-balance-mode-map (make-sparse-keymap)
  "Keymap for `global-balance-mode'.")

(defun balance-mode-on ()
  "Turn on `balance-mode' according to `balance-mode-context'."
  (let ((context (balance-mode-context)))
    (cond ((eq context 'balance-mode) (balance-mode))
          ((eq context 'balance-weight-mode) (balance-weight-mode)))))

(define-globalized-minor-mode global-balance-mode balance-mode balance-mode-on
  :group 'user)

(push '(global-balance-mode . ((:eval (unless (or balance-mode
                                                  balance-weight-mode)
                                        balance-mode-lighter-string))))
      minor-mode-alist)
(push `(global-balance-mode . ,global-balance-mode-map) minor-mode-map-alist)

(push '(balance-mode . 15) mode-line-minor-mode-priority-alist)
(push '(balance-weight-mode . 15) mode-line-minor-mode-priority-alist)
(push '(global-balance-mode . 15) mode-line-minor-mode-priority-alist)

(defun balance-mode-add-to-map-alist (assoc)
  "Add ASSOC to `balance-mode-map-alist'.
`balance-mode-map-alist' will be initialized for each
buffer to keep buffer local values.
This function set default value of `balance-mode-map-alist',
and set up advice to add ASSOC when initialization."
  (setq-default balance-mode-map-alist (cons assoc balance-mode-map-alist))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (local-variable-p 'balance-mode-map-alist)
          (push assoc balance-mode-map-alist))))

  (advice-add 'balance-mode-clean-up-keys :after
              (lambda (&rest _) (push assoc balance-mode-map-alist))))

(defvar balance-mode-universal-argument nil
  "Transiently t very after `universal-argument--mode' is called.")

(defvar balance-mode-universal-argument-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'universal-argument-more)
    map)
  "`universal-argument-map' but for `balance-mode'.")

(advice-add
 'universal-argument--mode :after
 (lambda (&rest _)
   (add-hook-for-once 'post-command-hook
                      (lambda () (setq balance-mode-universal-argument nil))
                      nil
                      nil
                      (lambda () (not (memq universal-argument-map
                                            overriding-terminal-local-map))))
   (setq balance-mode-universal-argument t)))

(balance-mode-add-to-map-alist
 `(balance-mode-universal-argument . ,balance-mode-universal-argument-map))

(balance-mode-add-to-map-alist
 `(consecutive-emulate-forward-backward-word-mode
   . ,consecutive-emulate-forward-backward-word-mode-map))

(balance-mode-add-to-map-alist
 `(consecutive-emulate-forward-backward-sexp-mode
   . ,consecutive-emulate-forward-backward-sexp-mode-map))

(balance-mode-add-to-map-alist
 `(consecutive-search-forward-char-in-line-mode
   . ,consecutive-search-forward-char-in-line-mode-map))

(balance-mode-add-to-map-alist
 `(consecutive-search-backward-char-in-line-mode
   . ,consecutive-search-backward-char-in-line-mode-map))

;; (advice-add 'manipulate-frame :around
;;             (lambda (function &rest args)
;;               (let ((binding (lookup-key overriding-balance-mode-map
;;                                          (kbd "q"))))
;;                 (unless (numberp binding)
;;                   (define-key overriding-balance-mode-map (kbd "q") nil))
;;                 (unwind-protect
;;                     (apply function args)
;;                   (unless (numberp binding)
;;                     (define-key overriding-balance-mode-map
;;                       (kbd "q") binding))))))

;; (advice-add 'manipulate-window :around
;;             (lambda (function &rest args)
;;               (let ((binding
;;                      (lookup-key overriding-balance-mode-map (kbd "q"))))
;;                 (unless (numberp binding)
;;                   (define-key overriding-balance-mode-map (kbd "q") nil))
;;                 (unwind-protect
;;                     (apply function args)
;;                   (unless (numberp binding)
;;                     (define-key overriding-balance-mode-map
;;                       (kbd "q") binding))))))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (string-equal (buffer-name) "*Messages*")
              (balance-mode-implement-keys
               (list (kbd "C-n")
                     (kbd "C-p")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-a")
                     (kbd "C-e")
                     (kbd "C-v")
                     (kbd "C-s")
                     (kbd "C-r")
                     (kbd "C-l C-c")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l n")
                     (kbd "C-l p")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l v")
                     (kbd "C-l C-l n")
                     (kbd "C-l C-l p")
                     (kbd "C-l C-l f")
                     (kbd "C-l C-l b")
                     (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x C-x")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1")
                     (kbd "C-x SPC")
                     (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c ,")
                     (kbd "C-c .")
                     (kbd "C-c h")
                     (kbd "C-c i d")
                     (kbd "C-c i f")
                     (kbd "C-c l m")
                     (kbd "C-c l n")
                     (kbd "C-c l b")
                     (kbd "C-c l v")
                     (kbd "C-c l i")
                     (kbd "C-c l f")
                     (kbd "C-c l c")
                     (kbd "C-c l l"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC n") . ,(kbd "l n"))
                 (,(kbd "l SPC p") . ,(kbd "l p"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC v") . ,(kbd "l v"))
                 (,(kbd "l l SPC n") . ,(kbd "l l n"))
                 (,(kbd "l l SPC p") . ,(kbd "l l p"))
                 (,(kbd "l l SPC f") . ,(kbd "l l f"))
                 (,(kbd "l l SPC b") . ,(kbd "l l b"))
                 (,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1"))
                 (,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l")))
               overriding-balance-weight-mode-map))))
(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode #'help-mode)
              (define-key overriding-balance-weight-mode-map
                (kbd "S") (key-binding (kbd "s")))
              (define-key overriding-balance-weight-mode-map
                (kbd "m") (kbd "RET"))
              (balance-mode-implement-keys
               (list (kbd "C-n")
                     (kbd "C-p")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-a")
                     (kbd "C-e")
                     (kbd "C-v")
                     (kbd "C-s")
                     (kbd "C-l C-c")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l n")
                     (kbd "C-l p")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l v")
                     (kbd "C-l C-l n")
                     (kbd "C-l C-l p")
                     (kbd "C-l C-l f")
                     (kbd "C-l C-l b")
                     (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x C-x")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1")
                     (kbd "C-x SPC")
                     (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c ,")
                     (kbd "C-c .")
                     (kbd "C-c h")
                     (kbd "C-c i d")
                     (kbd "C-c i f")
                     (kbd "C-c l m")
                     (kbd "C-c l n")
                     (kbd "C-c l b")
                     (kbd "C-c l v")
                     (kbd "C-c l i")
                     (kbd "C-c l f")
                     (kbd "C-c l c")
                     (kbd "C-c l l"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1"))
                 (,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC n") . ,(kbd "l n"))
                 (,(kbd "l SPC p") . ,(kbd "l p"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC v") . ,(kbd "l v"))
                 (,(kbd "l l SPC n") . ,(kbd "l l n"))
                 (,(kbd "l l SPC p") . ,(kbd "l l p"))
                 (,(kbd "l l SPC f") . ,(kbd "l l f"))
                 (,(kbd "l l SPC b") . ,(kbd "l l b"))
                 (,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l")))
               overriding-balance-weight-mode-map))))
(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode 'emacs-lisp-compilation-mode)
              (balance-mode-implement-keys
               (list (kbd "C-n")
                     (kbd "C-p")
                     (kbd "C-f")
                     (kbd "C-b")
                     (kbd "C-a")
                     (kbd "C-e")
                     (kbd "C-v")
                     (kbd "C-s")
                     (kbd "C-r")
                     (kbd "C-l a")
                     (kbd "C-l e")
                     (kbd "C-l n")
                     (kbd "C-l p")
                     (kbd "C-l f")
                     (kbd "C-l b")
                     (kbd "C-l v")
                     (kbd "C-l C-l n")
                     (kbd "C-l C-l p")
                     (kbd "C-l C-l f")
                     (kbd "C-l C-l b")
                     (kbd "C-l C-c")
                     (kbd "C-c ;")
                     (kbd "C-c :")
                     (kbd "C-c .")
                     (kbd "C-c ,")
                     (kbd "C-x C-f")
                     (kbd "C-x C-c")
                     (kbd "C-x C-x")
                     (kbd "C-x d")
                     (kbd "C-x b")
                     (kbd "C-x k")
                     (kbd "C-x 1")
                     (kbd "C-x SPC")
                     (kbd "C-c h")
                     (kbd "C-c i d")
                     (kbd "C-c i f")
                     (kbd "C-c l m")
                     (kbd "C-c l n")
                     (kbd "C-c l b")
                     (kbd "C-c l v")
                     (kbd "C-c l i")
                     (kbd "C-c l f")
                     (kbd "C-c l c")
                     (kbd "C-c l l"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               `((,(kbd "l SPC a") . ,(kbd "l a"))
                 (,(kbd "l SPC e") . ,(kbd "l e"))
                 (,(kbd "l SPC n") . ,(kbd "l n"))
                 (,(kbd "l SPC p") . ,(kbd "l p"))
                 (,(kbd "l SPC f") . ,(kbd "l f"))
                 (,(kbd "l SPC b") . ,(kbd "l b"))
                 (,(kbd "l SPC v") . ,(kbd "l v"))
                 (,(kbd "l l SPC n") . ,(kbd "l l n"))
                 (,(kbd "l l SPC p") . ,(kbd "l l p"))
                 (,(kbd "l l SPC f") . ,(kbd "l l f"))
                 (,(kbd "l l SPC b") . ,(kbd "l l b"))
                 (,(kbd "x SPC d") . ,(kbd "x d"))
                 (,(kbd "x SPC b") . ,(kbd "x b"))
                 (,(kbd "x SPC k") . ,(kbd "x k"))
                 (,(kbd "x SPC 1") . ,(kbd "x 1"))
                 (,(kbd "c SPC ;") . ,(kbd "c ;"))
                 (,(kbd "c SPC :") . ,(kbd "c :"))
                 (,(kbd "c SPC ,") . ,(kbd "c ,"))
                 (,(kbd "c SPC .") . ,(kbd "c ."))
                 (,(kbd "c SPC h") . ,(kbd "c h"))
                 (,(kbd "c SPC i") . ,(kbd "c i"))
                 (,(kbd "c SPC l") . ,(kbd "c l")))
               overriding-balance-weight-mode-map))))

(defcustom balance-mode-active-cursor-color (face-attribute 'cursor :background)
  "Cursor color used while `balance-mode' is activated."
  :group 'user
  :type 'color)

(defcustom balance-mode-semi-active-cursor-color
  (face-attribute 'cursor :background)
  "Cursor color used while `balance-weight-mode' is activated."
  :group 'user
  :type 'color)

(defcustom balance-mode-inactive-cursor-color
  (face-attribute 'cursor :background)
  "Cursor color used while `balance-mode' is not activated."
  :group 'user
  :type 'color)

(defun balance-mode-update-cursor-color (&optional force)
  "Update cursor color according to the state of `balance-mode'."
  (interactive "P")
  (let ((current (frame-parameter nil 'cursor-color))
        (trigger (cond (balance-mode
                        `(,balance-mode-inactive-cursor-color
                          ,balance-mode-semi-active-cursor-color))
                       (balance-weight-mode
                        `(,balance-mode-inactive-cursor-color
                          ,balance-mode-active-cursor-color))
                       (t `(,balance-mode-semi-active-cursor-color
                            ,balance-mode-active-cursor-color))))
        (intent (cond (balance-mode balance-mode-active-cursor-color)
                      (balance-weight-mode
                       balance-mode-semi-active-cursor-color)
                      (t balance-mode-inactive-cursor-color))))
    (if (or force (member current trigger))
        (set-frame-parameter nil 'cursor-color intent))))

(add-hook 'global-balance-mode-hook
          (lambda ()
            (if global-balance-mode
                (add-hook 'post-command-hook #'balance-mode-update-cursor-color)
              (remove-hook 'post-command-hook
                           #'balance-mode-update-cursor-color))
            (balance-mode-update-cursor-color)))

(add-hook
 'balance-mode-hook
 (lambda ()
   (if (string-equal (buffer-name) "*scratch*")
       (cond (balance-mode
              (setq default-frame-alist
                    (assq-delete-all 'cursor-color default-frame-alist))
              (push `(cursor-color . ,balance-mode-active-cursor-color)
                    default-frame-alist))
             (balance-weight-mode
              (setq default-frame-alist
                    (assq-delete-all 'cursor-color default-frame-alist))
              (push `(cursor-color . ,balance-mode-semi-active-cursor-color)
                    default-frame-alist))
             (t (setq default-frame-alist
                      (assq-delete-all 'cursor-color default-frame-alist))
                (push `(cursor-color . ,balance-mode-inactive-cursor-color)
                      default-frame-alist))))))

(add-hook 'global-balance-mode-hook
          (lambda ()
            (unless global-balance-mode
              (setq default-frame-alist
                    (assq-delete-all 'cursor-color default-frame-alist)))))

(run-with-idle-timer 0.2 t #'balance-mode-update-cursor-color)

(overriding-set-key (kbd "ESC M-SPC") #'global-balance-mode)


(resolve bindings)
