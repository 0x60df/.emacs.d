
;;;; init-cc-mode.el



;;; base

(eval-when-compile (require 'cc-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "gnu")
	    (setq c-hungry-delete-key t)))


;;; bindings

(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map "\C-ccc" 'compile)
            (define-key c-mode-map "\C-ccb" 'gdb)))

(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map "\C-ccc" 'compile)
            (define-key c++-mode-map "\C-ccb" 'gdb)
            (define-key c++-mode-map (kbd "C-c :") 'manipulate-window)))
