;;; -*- lexical-binding: t -*-
;;;; init-multiple-cursors.el


(premise init)
(premise custom)
(premise frame)
(premise mode-line)
(premise bindings)
(premise feature)
(premise inst-multiple-cursors)

(eval-when-compile (require 'multiple-cursors))

(lazy-autoload 'mc/cycle-forward "mc-cycle-cursors")
(lazy-autoload 'mc/cycle-backward "mc-cycle-cursors")

(declare-function disable-mc-before-make-frame load-file-name t t)

(custom-set-variables
 '(mc/always-run-for-all t)
 '(mc/max-cursors 32)
 `(mc/mode-line
   '(" MC:" (:eval (format ,(propertize "%d" 'face '(bold mode-line-warning))
                           (mc/num-cursors))))))

(push '(multiple-cursors-mode . 21) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'multiple-cursors-core
  (defun disable-mc-before-make-frame (&rest _args)
  "Advising `make-frame' to disable mc."
  (if multiple-cursors-mode
      (multiple-cursors-mode 0)))

  (advice-add 'make-frame :before #'disable-mc-before-make-frame)

  (defvar mc-standard-cursor-color nil
    "Temporary store for standard cursor color.")

  (defcustom mc-mode-cursor-color (face-attribute 'cursor :background)
    "Cursor color for `multiple-cursors-mode'."
    :group 'user
    :type 'color)
  (defvar mc-mode-cursor-color)

  (defcustom mc-mode-with-balance-mode-cursor-color
    (face-attribute 'cursor :background)
    "Cursor color for `multiple-cursors-mode' with `balance-mode'."
    :group 'user
    :type 'color)
  (defvar mc-mode-with-balance-mode-cursor-color)

  (let ((set-cursor-color
         (lambda ()
           (set-frame-parameter
            nil 'cursor-color
            (if (or balance-mode balance-weight-mode)
                mc-mode-with-balance-mode-cursor-color
              mc-mode-cursor-color)))))
    (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (if multiple-cursors-mode
                  (progn
                    (unless mc-standard-cursor-color
                      (setq mc-standard-cursor-color
                            (frame-parameter nil 'cursor-color)))
                    (funcall set-cursor-color)
                    (add-hook 'balance-mode-hook set-cursor-color)
                    (add-hook 'balance-weight-mode-hook set-cursor-color))
                (when mc-standard-cursor-color
                  (remove-hook 'balance-mode-hook set-cursor-color)
                  (remove-hook 'balance-weight-mode-hook set-cursor-color)
                  (set-frame-parameter
                   nil 'cursor-color mc-standard-cursor-color)
                  (setq mc-standard-cursor-color nil))))))

  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (unless multiple-cursors-mode
                (unless transient-mark-mode
                  (transient-mark-mode 1)))))

  (add-to-list 'mc/unsupported-minor-modes 'show-paren-mode)
  (with-eval-after-load 'company
    (add-to-list 'mc/unsupported-minor-modes 'company-split-mode))
  (with-eval-after-load 'visible-mark
    (add-to-list 'mc/unsupported-minor-modes 'visible-mark-mode)))

(overriding-set-key (kbd "C-c @ @") #'mc/mark-all-dwim)
(overriding-set-key (kbd "C-c @ e") #'mc/edit-lines)
(overriding-set-key (kbd "C-c @ n") #'mc/mark-next-like-this)
(overriding-set-key (kbd "C-c @ p") #'mc/mark-previous-like-this)
(overriding-set-key (kbd "C-c @ a") #'mc/mark-all-like-this)
(overriding-set-key (kbd "C-c @ C-v") #'mc/cycle-forward)
(overriding-set-key (kbd "C-c @ M-v") #'mc/cycle-backward)

(add-to-list 'balance-mode-key-list (kbd "C-c @ @"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ e"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ n"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ p"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ a"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ C-v"))
(add-to-list 'balance-mode-key-list (kbd "C-c @ M-v"))

(add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC @") . ,(kbd "c @")))

(add-hook 'multiple-cursors-mode-hook #'balance-mode-update-keys)


(resolve init-multiple-cursors)
