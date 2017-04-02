
;;;; init-expand-region.el



;;; base

(premise init)
(premise inst-expand-region)

(transient-mark-mode t)


;;; bindings

(global-set-key (kbd "C-`") 'er/expand-region)
(global-set-key (kbd "C-M-`") 'er/contract-region)


(resolve init-expand-region)
