
;;;; init-savehist.el


(premise init)
(premise inst-savehist)

(add-hook 'emacs-startup-hook #'savehist-mode)


(resolve init-savehist)
