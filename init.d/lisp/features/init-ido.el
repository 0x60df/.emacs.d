
;;;; init-ido.el



;;; base

(eval-and-compile (require 'ido))

(eval-after-load 'ido
  '(custom-set-variables '(ido-enable-flex-matching t)
                         '(ido-auto-merge-work-directories-length -1)))

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (set (make-local-variable 'truncate-lines) t)))


;;; bindings

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map " " #'ido-next-match)
            (define-key ido-completion-map (kbd "S-SPC") #'ido-prev-match)
            (define-key ido-completion-map (kbd "C-<tab>")
              (lambda()
                (interactive)
                (ido-initiate-auto-merge (current-buffer))))))
