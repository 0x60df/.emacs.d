
;;;; init-shifter.el


(premise init)

(global-set-key (kbd "C-c m") 'shifter-shift-major-mode)
(global-set-key (kbd "C-l m") 'shifter-shift-minor-mode)
(global-set-key (kbd "s-m e") 'shifter-turn-on-minor-mode)
(global-set-key (kbd "s-m d") 'shifter-turn-off-minor-mode)

(with-eval-after-load 'shifter
  (defvar shifter-hist-auto-save-timer nil
    "Timer used to automatically save the shifter hist.")
  (setq shifter-hist-auto-save-timer
        (run-with-idle-timer (* 60 63) t 'shifter-save-hist)))


(resolve shifter)
