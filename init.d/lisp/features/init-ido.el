
;;;; init-ido.el


(premise init)
(premise custom)

(eval-when-compile (require 'ido))

(declare-function ido-everywhere "ido")
(declare-function ido-next-match "ido")
(declare-function ido-prev-match "ido")
(declare-function ido-initiate-auto-merge "ido")

(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-auto-merge-work-directories-length -1))

(with-eval-after-load 'ido
  (add-hook 'ido-minibuffer-setup-hook
            (lambda () (set (make-local-variable 'truncate-lines) t)))
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-common-completion-map
                (kbd "SPC") #'ido-next-match)
              (define-key ido-common-completion-map
                (kbd "S-SPC") #'ido-prev-match))))

(add-hook 'emacs-startup-hook #'ido-everywhere)
(add-hook 'emacs-startup-hook #'ido-mode)


(resolve init-ido)
