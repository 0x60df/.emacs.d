
;;;; init-yasnippet.el



;;; base

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
(setq yas-prompt-functions nil)         ;force first match for same keyword
