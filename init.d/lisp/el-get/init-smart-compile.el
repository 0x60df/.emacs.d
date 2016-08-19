
;;;; init-smart-compile.el



;;; base

(eval-when-compile
  (require 'cc-mode)
  (require 'ruby-mode))


;;; bindings

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map "\C-ccs" 'smart-compile)))
(eval-after-load 'cc-mode
  '(progn
     (define-key c++-mode-map "\C-ccs" 'smart-compile)))
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-ccs" 'smart-compile)))
