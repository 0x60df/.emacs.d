
;;;; init-multiple-cursors.el



;;; base

(eval-when-compile (require 'org))

(require 'multiple-cursors)
(global-set-key (kbd "C-@") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c @ e") 'mc/edit-lines)
(global-set-key (kbd "C-c @ n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c @ p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c @ a") 'mc/mark-all-like-this)

(eval-after-load 'org
  (define-key org-mode-map (kbd "C-c @") nil))
