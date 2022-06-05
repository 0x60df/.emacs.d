
;;;; init-scratchb.el


(premise init)
(premise user-feature)

(eval-when-compile (require 'scratchb))

(add-hook 'emacs-startup-hook #'scratchb-sticky-mode)
(add-hook 'emacs-startup-hook #'scratchb-auto-revert-mode)
(add-hook 'emacs-startup-hook #'scratchb-auto-snapshot-mode)

(with-eval-after-load 'scratchb
  (defvar scratchb-auto-snapshot-timer nil
    "Timer for taking snapshot of *scratchb* automatically.")
  (setq scratchb-auto-snapshot-timer
        (run-with-idle-timer (* 60 62) t #'scratchb-snapshot))

  (define-key scratchb-mode-map (kbd "C-c k") #'scratchb-flush)

  (add-to-list 'balance-mode-key-list (kbd "C-c k"))
  (add-to-list 'balance-mode-key-alias-alist
               `(,(kbd "c SPC k") . ,(kbd "c k"))))


(resolve init-scratchb)
