
;;;; init-org.el


(premise init)

(eval-when-compile (require 'org))

(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-coa" 'org-agenda)
(global-set-key "\C-coc" 'org-capture)
(global-set-key "\C-cob" 'org-iswitchb)
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-,") nil)
     (define-key org-mode-map (kbd "C-c ;") nil)
     (define-key org-mode-map (kbd "C-c :") nil)))


(resolve init-org)
