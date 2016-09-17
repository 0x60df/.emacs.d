
;;;; init-org.el


(eval-when-compile (require 'org))

(define-key global-map "\C-col" 'org-store-link)
(define-key global-map "\C-coa" 'org-agenda)
(define-key global-map "\C-coc" 'org-capture)
(define-key global-map "\C-cob" 'org-iswitchb)
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-,") nil)
     (define-key org-mode-map (kbd "C-c ;") nil)
     (define-key org-mode-map (kbd "C-c :") nil)))
