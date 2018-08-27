
;;;; init-recentf.el


(premise init)

(eval-after-load 'recentf
  '(progn
     (defvar recentf-auto-save-timer nil
       "Timer used to automatically save the recent list.")
     (custom-set-variables '(recentf-max-saved-items 1000)
                           '(recentf-auto-cleanup 30))
     (setq recentf-auto-save-timer
           (run-with-idle-timer 60 t 'recentf-save-list))))


(resolve init-recentf)
