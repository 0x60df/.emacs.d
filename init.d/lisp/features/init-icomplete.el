
;;;; init-icomplete.el


(premise init)
(premise custom)

(eval-when-compile (require 'icomplete))

(declare-function icomplete-forward-completions "icomplete")
(declare-function icomplete-backward-completions "icomplete")

(custom-set-variables
 '(icomplete-prospects-height 1))

(with-eval-after-load 'icomplete
  (icomplete-mode 0)
  (define-key icomplete-minibuffer-map (kbd "SPC")
    #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "S-SPC")
    #'icomplete-backward-completions))


(resolve init-icomplete)
