
;;;; init-vertico.el


(premise init)
(premise inst-vertico)

(defvar vertico-multiform-commands)

(add-hook 'emacs-startup-hook #'vertico-mode)
(setq enable-recursive-minibuffers t)

(add-hook 'emacs-startup-hook #'vertico-multiform-mode)
(with-eval-after-load 'vertico-multiform
  (dolist (command '(loophole-register
                     loophole-unregister
                     loophole-prioritize
                     loophole-globalize
                     loophole-localize
                     loophole-enable
                     loophole-disable
                     loophole-name
                     loophole-tag
                     loophole-start-editing
                     loophole-start-timer
                     loophole-stop-timer
                     loophole-extend-timer
                     loophole-describe
                     loophole-merge
                     loophole-duplicate))
    (add-to-list 'vertico-multiform-commands
                 `(,command (vertico-sort-function . identity)))))

(with-eval-after-load 'consult
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))


(resolve init-vertico)
