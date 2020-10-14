
;;;; init-scratchb.el


(premise init)

(require 'scratchb)

(scratchb-auto-revert-mode 1)
(scratchb-auto-snapshot-mode 1)

(add-hook 'emacs-startup-hook #'scratchb-mode-buffer-sticky)
(add-hook 'scratchb-after-revert-hook #'scratchb-mode-buffer-sticky)

(define-key scratchb-mode-map (kbd "C-c k") #'scratchb-flush)


(resolve scratchb)
