
;;;; init-ido.el


(ido-mode t)
(require 'ido)
(setq ido-enable-flex-matching t)

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (set (make-local-variable 'truncate-lines) t)))
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map " " 'ido-next-match)
            (define-key ido-completion-map (kbd "S-SPC") 'ido-prev-match)))
