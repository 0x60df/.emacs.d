
;;;; init-yasnippet.el



;;; base

(premise init)
(premise inst-yasnippet)

(require 'yasnippet)

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
(custom-set-variables '(yas-prompt-functions nil)
                      '(yas-use-menu nil))


(resolve init-yasnippet)
