
;;;; init-shifter.el


(premise init)
(premise user-feature)
(premise bindings)

(declare-function shifter-save-hist "shifter")

(overriding-set-key (kbd "C-c m") #'shifter-shift-major-mode)
(overriding-set-key (kbd "C-l m") #'shifter-shift-minor-mode)

(with-eval-after-load 'shifter
  (defvar shifter-hist-auto-save-timer nil
    "Timer used to automatically save the shifter hist.")
  (setq shifter-hist-auto-save-timer
        (run-with-idle-timer (* 60 63) t #'shifter-save-hist)))


(resolve shifter)
