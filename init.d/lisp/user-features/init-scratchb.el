
;;;; init-scratchb.el


(premise init)
(premise user-feature)

(eval-when-compile (require 'scratchb))

(add-hook 'emacs-startup-hook #'scratchb-auto-revert-mode)
(add-hook 'emacs-startup-hook #'scratchb-auto-snapshot-mode)
(add-hook 'emacs-startup-hook #'scratchb-mode-buffer-sticky)

(with-eval-after-load 'scratchb
  (defvar scratchb-auto-snapshot-timer nil
    "Timer for taking snapshot of *scratchb* automatically.")
  (setq scratchb-auto-snapshot-timer
        (run-with-idle-timer (* 60 62) t #'scratchb-snapshot))

  (add-hook 'scratchb-after-revert-hook #'scratchb-mode-buffer-sticky)

  (define-key scratchb-mode-map (kbd "C-c k") #'scratchb-flush))


(resolve init-scratchb)
