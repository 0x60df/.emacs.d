
;;;; init-skk.el



;;; base

(premise init)
(premise frame)

(require 'skk-autoloads)


;;; bindings

(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key (kbd "s-\\") 'skk-mode)
(global-set-key (kbd "C-<zenkaku-hankaku>") 'skk-mode)
(global-set-key (kbd "C-<hiragana-katakana>") 'skk-mode)
(setq skk-user-directory "~/.emacs.d/ddskk")
(eval-after-load 'skk-vars
  '(custom-set-variables '(skk-isearch-mode-enable nil)
                         '(skk-byte-compile-init-file t)))

(defadvice skk-mode (before bind-modifier)
  (define-key key-translation-map (kbd "<muhenkan>")
            (if skk-mode
                nil
              (kbd "C-j"))))
(ad-activate 'skk-mode)


;;; multiple terminal enviroment

(defadvice skk-cursor-set-1 (around get-terminal-cursor-color)
  (let ((ccc-default-cursor-color
         (terminal-parameter nil 'ccc-terminal-cursor-color)))
    ad-do-it))
(defadvice skk-cursor-off-1 (around get-terminal-cursor-color)
  (let ((ccc-default-cursor-color
         (terminal-parameter nil 'ccc-terminal-cursor-color)))
    ad-do-it))
(defadvice ccc-default-cursor-color (around get-terminal-cursor-color)
  (let ((ccc-default-cursor-color
         (terminal-parameter nil 'ccc-terminal-cursor-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-cursor-color)
  (let ((ccc-default-cursor-color
          (terminal-parameter frame 'ccc-terminal-cursor-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-cursor-color)
  (setq ccc-default-cursor-color
        (let ((ccc-default-cursor-color
                (terminal-parameter nil 'ccc-terminal-cursor-color)))
          ad-do-it
          ccc-default-cursor-color)))

(defadvice ccc-setup (after set-terminal-cursor-color)
  (set-terminal-parameter
   nil 'ccc-terminal-cursor-color ccc-default-cursor-color))
(defadvice ccc-setup-current-colors (after set-terminal-cursor-color)
  (set-terminal-parameter
   nil 'ccc-terminal-cursor-color ccc-default-cursor-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-cursor-color)
  (set-terminal-parameter
   nil 'ccc-terminal-cursor-color ccc-default-cursor-color))

(defadvice ccc-default-foreground-color (around get-terminal-foreground-color)
  (let ((ccc-default-foreground-color
         (terminal-parameter nil 'ccc-terminal-foreground-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-foreground-color)
  (let ((ccc-default-foreground-color
          (terminal-parameter frame 'ccc-terminal-foreground-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-foreground-color)
  (setq ccc-default-foreground-color
        (let ((ccc-default-foreground-color
               (terminal-parameter nil 'ccc-terminal-foreground-color)))
          ad-do-it
          ccc-default-foreground-color)))

(defadvice ccc-setup (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'ccc-terminal-foreground-color ccc-default-foreground-color))
(defadvice ccc-setup-current-colors (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'ccc-terminal-foreground-color ccc-default-foreground-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'ccc-terminal-foreground-color ccc-default-foreground-color))

(defadvice ccc-default-background-color (around get-terminal-background-color)
  (let ((ccc-default-background-color
         (terminal-parameter nil 'ccc-terminal-background-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-background-color)
  (let ((ccc-default-background-color
         (terminal-parameter frame 'ccc-terminal-background-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-background-color)
  (setq ccc-default-background-color
        (let ((ccc-default-background-color
                (terminal-parameter nil 'ccc-terminal-background-color)))
          ad-do-it
          ccc-default-background-color)))

(defadvice ccc-setup (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'ccc-terminal-background-color ccc-default-background-color))
(defadvice ccc-setup-current-colors (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'ccc-terminal-background-color ccc-default-background-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'ccc-terminal-background-color ccc-default-background-color))

(mapc (lambda (f) (ad-activate f))
      '(skk-cursor-set-1
        skk-cursor-off-1
        ccc-default-cursor-color
        ccc-setup-new-frame
        ccc-setup
        ccc-setup-current-colors
        custom-theme-checkbox-toggle
        ccc-default-foreground-color
        ccc-default-background-color))

(autoload 'ccc-setup-current-colors "ccc")
(autoload 'ccc-setup-new-frame "ccc")

(defun ccc-setup-new-terminal-function ()
  (add-hook 'post-command-hook 'ccc-update-buffer-local-frame-params)
  (add-hook 'after-make-frame-functions 'ccc-setup-new-frame)
  (ccc-setup-current-colors)
  (ccc-setup-new-frame (selected-frame))
  (remove-hook 'pre-command-hook #'ccc-setup-new-terminal-function))
(add-hook 'after-make-terminal-functions
          (lambda (terminal)
            (add-hook 'pre-command-hook #'ccc-setup-new-terminal-function)))


;;; server

(defadvice server-create-window-system-frame
    (after ccc-update-buffer-local-frame-params)
  (ccc-update-buffer-local-frame-params))
(ad-activate 'server-create-window-system-frame)


;;; evil

(eval-after-load 'skk
  '(eval-after-load 'evil
     '(progn
        (defadvice update-buffer-local-cursor-color (around suppress-on-evil)
          (if (and (eq evil-state 'emacs) skk-mode)
              ad-do-it))
        (defadvice evil-refresh-cursor (around suppress-on-skk)
          (if (not (and (eq evil-state 'emacs) skk-mode))
              ad-do-it))
        (ad-activate 'update-buffer-local-cursor-color)
        (ad-activate 'evil-refresh-cursor))))
