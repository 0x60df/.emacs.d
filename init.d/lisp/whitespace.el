
;;;; whitespace.el


(premise init)
(premise custom)

(custom-set-variables
 '(show-trailing-whitespace t))

(add-hook 'messages-buffer-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(with-current-buffer "*Messages*"
  (setq show-trailing-whitespace nil))


(resolve whitespace)
