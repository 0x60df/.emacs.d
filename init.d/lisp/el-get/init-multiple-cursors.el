
;;;; init-multiple-cursors.el



;;; base

(premise init)
(premise inst-multiple-cursors)

(eval-when-compile (require 'org))

(require 'multiple-cursors)

(global-set-key (kbd "C-@") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c @ e") 'mc/edit-lines)
(global-set-key (kbd "C-c @ n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c @ p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c @ a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c @ SPC") 'set-mark-command)

(add-to-list 'mc/unsupported-minor-modes 'visible-mark-mode)
(add-to-list 'mc/unsupported-minor-modes 'show-paren-mode)

(custom-set-variables '(mc/always-run-for-all t))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c @") nil))


(resolve init-multiple-cursors)
