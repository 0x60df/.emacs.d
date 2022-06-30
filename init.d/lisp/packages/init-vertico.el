
;;;; init-vertico.el


(premise init)
(premise inst-vertico)

(add-hook 'emacs-startup-hook #'vertico-mode)
(setq enable-recursive-minibuffers t)

(with-eval-after-load 'consult
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'#'completion--in-region)
                 args))))



(resolve init-vertico)
