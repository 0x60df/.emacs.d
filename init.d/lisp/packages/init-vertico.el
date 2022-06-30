
;;;; init-vertico.el


(premise init)
(premise inst-vertico)

(add-hook 'emacs-startup-hook #'vertico-mode)
 (setq enable-recursive-minibuffers t)


(resolve init-vertico)
