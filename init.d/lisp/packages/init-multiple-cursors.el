
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

(custom-set-variables
 '(mc/always-run-for-all t)
 `(mc/mode-line
   '(" MC:" (:eval (format ,(propertize "%d" 'face '(bold mode-line-warning))
                           (mc/num-cursors))))))

(push '(multiple-cursors-mode . 23) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-@") #'mc/mark-all-dwim)
(overriding-set-key (kbd "C-c @ e") #'mc/edit-lines)
(overriding-set-key (kbd "C-c @ n") #'mc/mark-next-like-this)
(overriding-set-key (kbd "C-c @ p") #'mc/mark-previous-like-this)
(overriding-set-key (kbd "C-c @ a") #'mc/mark-all-like-this)
(overriding-set-key (kbd "C-c @ SPC") #'set-mark-command)
(overriding-set-key (kbd "C-c @ C-v") #'mc/cycle-forward)
(overriding-set-key (kbd "C-c @ M-v") #'mc/cycle-backward)

(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc/unsupported-minor-modes 'show-paren-mode)
  (with-eval-after-load 'visible-mark
    (add-to-list 'mc/unsupported-minor-modes 'visible-mark-mode)))


(resolve init-multiple-cursors)
