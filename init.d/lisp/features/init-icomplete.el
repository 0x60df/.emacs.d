
;;;; init-icomplete.el



;;; base

(eval-when-compile (require 'icomplete))
(icomplete-mode 1)
(custom-set-variables '(icomplete-prospects-height 1))


;;; bindings

(eval-after-load 'icomplete
  '(progn
     (define-key icomplete-minibuffer-map " "
       'icomplete-forward-completions)
     (define-key icomplete-minibuffer-map [return]
       'minibuffer-force-complete-and-exit)))
