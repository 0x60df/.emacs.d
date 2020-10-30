
;;;; init-scratchb.el


(premise init)
(premise user-feature)

(require 'scratchb)

(scratchb-auto-revert-mode)
(scratchb-auto-snapshot-mode)

(add-hook 'emacs-startup-hook #'scratchb-mode-buffer-sticky)
(add-hook 'scratchb-after-revert-hook #'scratchb-mode-buffer-sticky)

(define-key scratchb-mode-map (kbd "C-c k") #'scratchb-flush)


(resolve scratchb)
