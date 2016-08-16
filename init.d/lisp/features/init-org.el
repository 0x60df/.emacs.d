
;;;; init-org.el


(eval-when-compile (require 'org))

(define-key global-map "\C-col" 'org-store-link)
(define-key global-map "\C-coa" 'org-agenda)
(define-key global-map "\C-coc" 'org-capture)
(define-key global-map "\C-cob" 'org-iswitchb)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-,") 'other-window)
            (define-key org-mode-map (kbd "C-c ;") 'manipulate-frame)
            (define-key org-mode-map (kbd "C-c :") 'manipulate-window)))
