
;;;; init-yasnippet.el



;;; base

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
;; force first match for same keyword
(custom-set-variables '(yas-prompt-functions nil))
