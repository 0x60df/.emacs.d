
;;;; init-scratch.el


(premise init)

(global-set-key (kbd "C-c m") 'shifter-shift-major-mode)
(global-set-key (kbd "C-l m") 'shifter-shift-minor-mode)
(global-set-key (kbd "s-m e") 'shifter-turn-on-minor-mode)
(global-set-key (kbd "s-m d") 'shifter-turn-off-minor-mode)

(setq shifter-hist-auto-save-timer
      (run-with-idle-timer 70 t 'shifter-save-hist))


(resolve scratch)
