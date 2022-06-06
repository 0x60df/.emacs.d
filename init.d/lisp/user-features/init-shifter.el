
;;;; init-shifter.el


(premise init)
(premise user-feature)
(premise bindings)

(declare-function shifter-save-hist "shifter")

(overriding-set-key (kbd "C-c m") #'shifter-shift-major-mode)
(overriding-set-key (kbd "C-c n") #'shifter-shift-minor-mode)

(add-to-list 'balance-mode-key-list (kbd "C-c m"))
(add-to-list 'balance-mode-key-list (kbd "C-c n"))

(add-to-list 'balance-mode-key-alias-alist
             `(,(kbd "c SPC m") . ,(kbd "c m")))
(add-to-list 'balance-mode-key-alias-alist
             `(,(kbd "c SPC n") . ,(kbd "c n")))

(with-eval-after-load 'shifter
  (shifter-non-volatile-hist-mode)

  (defvar shifter-hist-auto-save-timer nil
    "Timer used to automatically save the shifter hist.")
  (setq shifter-hist-auto-save-timer
        (run-with-idle-timer (* 60 63) t #'shifter-save-hist)))


(resolve init-shifter)
