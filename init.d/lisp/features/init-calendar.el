
;;;; init-calendar.el


(premise init)

(add-hook 'calendar-mode-hook (lambda () (setq show-trailing-whitespace nil)))


(resolve init-calendar)
