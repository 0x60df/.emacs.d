
;;;; whitespace.el


(premise init)
(premise custom)

(custom-set-variables
 '(show-trailing-whitespace t))
(mapc (lambda (hook)
        (add-hook hook (lambda () (setq show-trailing-whitespace nil))))
      '(term-mode-hook
        eshell-mode-hook
        eww-mode-hook
        calendar-mode-hook))


(resolve whitespace)
