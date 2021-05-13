
;;;; init-multiple-cursors.el


(premise init)
(premise custom)
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

(push '(multiple-cursors-mode . 23) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'multiple-cursors-core
  (defun disable-mc-before-make-frame (&rest args)
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

  (advice-add-for-once 'multiple-cursors-mode
                       :before (lambda (&rest _)
                                 (setq mc-standard-cursor-color
                                       (frame-parameter nil 'cursor-color))))

  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (if multiple-cursors-mode
                  (set-frame-parameter nil 'cursor-color mc-mode-cursor-color)
                (if mc-standard-cursor-color
                    (set-frame-parameter
                     nil 'cursor-color mc-standard-cursor-color)))))

  (add-to-list 'mc/unsupported-minor-modes 'show-paren-mode)
  (with-eval-after-load 'company
    (add-to-list 'mc/unsupported-minor-modes 'company-split-mode))
  (with-eval-after-load 'visible-mark
    (add-to-list 'mc/unsupported-minor-modes 'visible-mark-mode)))

(overriding-set-key (kbd "C-@") #'mc/mark-all-dwim)
(overriding-set-key (kbd "C-c @ e") #'mc/edit-lines)
(overriding-set-key (kbd "C-c @ n") #'mc/mark-next-like-this)
(overriding-set-key (kbd "C-c @ p") #'mc/mark-previous-like-this)
(overriding-set-key (kbd "C-c @ a") #'mc/mark-all-like-this)
(overriding-set-key (kbd "C-c @ SPC") #'set-mark-command)
(overriding-set-key (kbd "C-c @ C-v") #'mc/cycle-forward)
(overriding-set-key (kbd "C-c @ M-v") #'mc/cycle-backward)


(resolve init-multiple-cursors)
