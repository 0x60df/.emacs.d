
;;;; whitespace.el


(setq-default show-trailing-whitespace t)
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace nil))))
      '(term-mode-hook
        eshell-mode-hook
        eww-mode-hook))
