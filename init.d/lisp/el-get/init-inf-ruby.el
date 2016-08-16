
;;;; init-inf-ruby.el



;;; base

(eval-when-compile (require 'ruby-mode))

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-ccr" 'run-ruby)))
