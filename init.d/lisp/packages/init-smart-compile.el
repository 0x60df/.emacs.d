
;;;; init-smart-compile.el



;;; base

(premise init)
(premise inst-smart-compile)

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


(resolve init-smart-compile)
