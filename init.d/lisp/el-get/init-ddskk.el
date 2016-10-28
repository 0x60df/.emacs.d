
;;;; init-skk.el


(premise init "~/.emacs.d/init.el")
(premise frame)
(premise client)

(require 'skk-autoloads)

(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key (kbd "s-\\") 'skk-mode)
(global-set-key (kbd "C-<zenkaku-hankaku>") 'skk-mode)
(global-set-key (kbd "C-<hiragana-katakana>") 'skk-mode)
(custom-set-variables
 '(skk-user-directory "~/.emacs.d/ddskk")
 '(skk-isearch-mode-enable nil)
 '(skk-byte-compile-init-file t))

(defadvice skk-mode (before bind-modifier)
  (define-key key-translation-map (kbd "<muhenkan>")
            (if skk-mode
                nil
              (kbd "C-j"))))
(ad-activate 'skk-mode)

(defadvice skk-cursor-set-1 (around get-terminal-cursor-color)
  (let ((default-cursor-color (terminal-parameter nil 'terminal-cursor-color)))
    ad-do-it))
(defadvice skk-cursor-off-1 (around get-terminal-cursor-color)
  (let ((default-cursor-color (terminal-parameter nil 'terminal-cursor-color)))
    ad-do-it))
(defadvice default-cursor-color (around get-terminal-cursor-color)
  (let ((default-cursor-color (terminal-parameter nil 'terminal-cursor-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-cursor-color)
  (let ((default-cursor-color
          (terminal-parameter frame 'terminal-cursor-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-cursor-color)
  (setq default-cursor-color
        (let ((default-cursor-color
                (terminal-parameter nil 'terminal-cursor-color)))
          ad-do-it
          default-cursor-color)))

(defadvice ccc-setup (after set-terminal-cursor-color)
  (set-terminal-parameter nil 'terminal-cursor-color default-cursor-color))
(defadvice ccc-setup-current-colors (after set-terminal-cursor-color)
  (set-terminal-parameter nil 'terminal-cursor-color default-cursor-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-cursor-color)
  (set-terminal-parameter nil 'terminal-cursor-color default-cursor-color))

(defadvice default-foreground-color (around get-terminal-foreground-color)
  (let ((default-foreground-color
          (terminal-parameter nil 'terminal-foreground-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-foreground-color)
  (let ((default-foreground-color
          (terminal-parameter frame 'terminal-foreground-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-foreground-color)
  (setq default-foreground-color
        (let ((default-foreground-color
                (terminal-parameter nil 'terminal-foreground-color)))
          ad-do-it
          default-foreground-color)))

(defadvice ccc-setup (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'terminal-foreground-color default-foreground-color))
(defadvice ccc-setup-current-colors (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'terminal-foreground-color default-foreground-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-foreground-color)
  (set-terminal-parameter
   nil 'terminal-foreground-color default-foreground-color))

(defadvice default-background-color (around get-terminal-background-color)
  (let ((default-background-color
         (terminal-parameter nil 'terminal-background-color)))
    ad-do-it))
(defadvice ccc-setup-new-frame (around get-terminal-background-color)
  (let ((default-background-color
         (terminal-parameter frame 'terminal-background-color)))
    ad-do-it))
(defadvice ccc-setup (around get-terminal-background-color)
  (setq default-background-color
        (let ((default-background-color
                (terminal-parameter nil 'terminal-background-color)))
          ad-do-it
          default-background-color)))

(defadvice ccc-setup (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'terminal-background-color default-background-color))
(defadvice ccc-setup-current-colors (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'terminal-background-color default-background-color))
(defadvice custom-theme-checkbox-toggle (after set-terminal-background-color)
  (set-terminal-parameter
   nil 'terminal-background-color default-background-color))

(mapc (lambda (f) (ad-activate f))
      '(skk-cursor-set-1
        skk-cursor-off-1
        default-cursor-color
        ccc-setup-new-frame
        ccc-setup
        ccc-setup-current-colors
        custom-theme-checkbox-toggle
        default-foreground-color
        default-background-color))

(autoload 'ccc-setup-current-colors "ccc")
(autoload 'ccc-setup-new-frame "ccc")

(defun ccc-setup-new-terminal-function ()
  (ccc-setup-current-colors)
  (ccc-setup-new-frame (selected-frame))
  (remove-hook 'pre-command-hook #'ccc-setup-new-terminal-function))
(add-hook 'after-make-terminal-functions
          (lambda (terminal)
            (add-hook 'pre-command-hook #'ccc-setup-new-terminal-function)))

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
