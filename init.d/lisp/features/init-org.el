
;;;; init-org.el


(eval-when-compile (require 'org))

(define-key global-map "\C-col" 'org-store-link)
(define-key global-map "\C-coa" 'org-agenda)
(define-key global-map "\C-coc" 'org-capture)
(define-key global-map "\C-cob" 'org-iswitchb)
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-,") 'other-window)
     (define-key org-mode-map (kbd "C-c ;") 'manipulate-frame)
     (define-key org-mode-map (kbd "C-c :") 'manipulate-window)))
(add-hook 'org-mode-hook
          (lambda ()
            (when (fboundp 'multiple-cursors-mode)
              (local-unset-key (kbd "C-c @"))
              (local-set-key (kbd "C-c @ e") 'mc/edit-lines)
              (local-set-key (kbd "C-c @ n") 'mc/mark-next-like-this)
              (local-set-key (kbd "C-c @ p") 'mc/mark-previous-like-this)
              (local-set-key (kbd "C-c @ a") 'mc/mark-all-like-this))))
