
;;;; init-recentf.el


(premise init)

(custom-set-variables '(recentf-max-saved-items 1000)
                      '(recentf-auto-cleanup (* 60 60)))

(with-eval-after-load 'recentf
  (defvar recentf-auto-save-timer nil
    "Timer used to automatically save the recent list.")
  (setq recentf-auto-save-timer
        (run-with-idle-timer (* 60 61) t 'recentf-save-list)))


(resolve init-recentf)
