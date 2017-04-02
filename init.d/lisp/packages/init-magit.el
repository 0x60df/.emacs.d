
;;;; init-magit.el



;;; base

(premise init)
(premise inst-magit)

(global-set-key "\C-cvm" 'magit-status)
(global-set-key "\C-cvc" 'magit-checkout)
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace nil))))
      '(magit-mode-hook
        magit-popup-mode-hook))


(resolve init-magit)