
;;;; init-yasnippet.el



;;; base

(premise init)
(premise inst-yasnippet)

(eval-when-compile (require 'yasnippet))

(yas-global-mode 1)

(eval-after-load 'yasnippet
  '(progn
     (define-key yas-minor-mode-map (kbd "<tab>") nil)
     (define-key yas-minor-mode-map (kbd "TAB") nil)
     (define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
     (setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
     ;; force first match for same keyword
     '(custom-set-variables '(yas-prompt-functions nil))))


(resolve init-yasnippet)
