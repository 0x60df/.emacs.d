
;;;; init-expand-region.el



;;; base

(transient-mark-mode t)


;;; bindings

(global-set-key (kbd "C-`") 'er/expand-region)
(global-set-key (kbd "C-s-`") 'er/contract-region)
