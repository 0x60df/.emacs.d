
;;;; init-magit.el



;;; base

(global-set-key "\C-cvm" 'magit-status)
(global-set-key "\C-cvc" 'magit-checkout)
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace nil))))
      '(magit-mode-hook
        magit-popup-mode-hook))
