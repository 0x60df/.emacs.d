
;;;; init-calendar.el


(premise init)

(declare-function calendar-mark-today "calendar")

(custom-set-variables
 '(calendar-mark-holidays-flag t))

(with-eval-after-load 'calendar
  (add-hook 'calendar-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))

(with-eval-after-load 'calendar
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))


(resolve init-calendar)
