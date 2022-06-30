
;;;; init-marginalia.el


(premise init)
(premise inst-marginalia)

(add-hook 'emacs-startup-hook #'marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)


(resolve init-marginalia)
