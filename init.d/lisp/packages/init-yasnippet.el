
;;;; init-yasnippet.el



;;; base

(premise init)
(premise inst-yasnippet)

(eval-when-compile (require 'yasnippet))

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")
(custom-set-variables '(yas-prompt-functions nil)
                      '(yas-use-menu nil))

(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer "*Messages*"
              (yas-minor-mode -1))))

(defun disable-yas-if-no-snippets ()
  (when (and yas-minor-mode (null (yas--get-snippet-tables)))
    (yas-minor-mode -1)))
(add-hook 'yas-minor-mode-hook #'disable-yas-if-no-snippets)


(resolve init-yasnippet)
