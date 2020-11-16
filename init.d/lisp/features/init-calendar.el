
;;;; init-calendar.el


(premise init)

(with-eval-after-load 'calendar
  (add-hook 'calendar-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))


(resolve init-calendar)
