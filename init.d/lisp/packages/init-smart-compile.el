
;;;; init-smart-compile.el


(premise init)
(premise inst-smart-compile)

(eval-when-compile
  (require 'cc-mode)
  (require 'ruby-mode))


(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "C-c c s") #'smart-compile))

(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c c s") #'smart-compile))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c s") #'smart-compile))


(resolve init-smart-compile)
