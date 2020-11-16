
;;;; init-scratchb.el


(premise init)
(premise user-feature)

(eval-when-compile (require 'scratchb))

(add-hook 'emacs-startup-hook #'scratchb-auto-revert-mode)
(add-hook 'emacs-startup-hook #'scratchb-auto-snapshot-mode)
(add-hook 'emacs-startup-hook #'scratchb-mode-buffer-sticky)

(with-eval-after-load 'scratchb
  (add-hook 'scratchb-after-revert-hook #'scratchb-mode-buffer-sticky)

  (define-key scratchb-mode-map (kbd "C-c k") #'scratchb-flush))


(resolve init-scratchb)
