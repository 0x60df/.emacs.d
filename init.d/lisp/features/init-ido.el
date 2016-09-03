
;;;; init-ido.el



;;; base

(ido-mode t)
(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (set (make-local-variable 'truncate-lines) t)))


;;; bindings

(define-key ido-common-completion-map " " #'ido-next-match)
(define-key ido-common-completion-map (kbd "S-SPC") #'ido-prev-match)
(define-key ido-file-dir-completion-map (kbd "C-<tab>")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))
