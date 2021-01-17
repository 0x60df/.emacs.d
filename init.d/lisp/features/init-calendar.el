
;;;; init-calendar.el


(premise init)
(premise mode-line)

(declare-function calendar-mark-today "calendar")

(custom-set-variables
 '(calendar-mark-holidays-flag t))

(with-eval-after-load 'calendar
  (add-hook 'calendar-mode-hook (lambda () (setq show-trailing-whitespace nil)))

  (advice-add 'calendar-set-mode-line :around #'enhance-mode-line-format)
  (advice-add 'calendar-update-mode-line :around #'enhance-mode-line-format)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))


(resolve init-calendar)
