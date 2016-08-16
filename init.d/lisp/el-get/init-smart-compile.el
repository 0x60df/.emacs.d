
;;;; init-smart-compile.el



;;; base

(eval-when-compile
  (require 'cc-mode)
  (require 'ruby-mode))


;;; bindings

(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map "\C-ccs" 'smart-compile)))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map "\C-ccs" 'smart-compile)))
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-ccs" 'smart-compile)))
