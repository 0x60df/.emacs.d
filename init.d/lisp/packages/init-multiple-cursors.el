
;;;; init-multiple-cursors.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-multiple-cursors)

(eval-when-compile (require 'multiple-cursors))

(custom-set-variables
 '(mc/always-run-for-all t))

(overriding-set-key (kbd "C-@") #'mc/mark-all-dwim)
(overriding-set-key (kbd "C-c @ e") #'mc/edit-lines)
(overriding-set-key (kbd "C-c @ n") #'mc/mark-next-like-this)
(overriding-set-key (kbd "C-c @ p") #'mc/mark-previous-like-this)
(overriding-set-key (kbd "C-c @ a") #'mc/mark-all-like-this)
(overriding-set-key (kbd "C-c @ SPC") #'set-mark-command)

(with-eval-after-load 'multiple-cursors
  (add-to-list 'mc/unsupported-minor-modes 'show-paren-mode)
  (eval-after-load 'visible-mark
    '(add-to-list 'mc/unsupported-minor-modes 'visible-mark-mode)))


(resolve init-multiple-cursors)
