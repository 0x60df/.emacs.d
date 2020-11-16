
;;;; init-anzu.el


(premise init)
(premise custom)
(premise bindings)
(premise mode-line)
(premise inst-anzu)

(custom-set-variables
 '(anzu-cons-mode-line-p nil)
 '(anzu-mode-lighter ""))

(overriding-set-key (kbd "M-%") #'anzu-query-replace)
(overriding-set-key (kbd "C-M-%") #'anzu-query-replace-regexp)
(define-key isearch-mode-map [remap isearch-query-replace]
  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp]
  #'anzu-isearch-query-replace-regexp)

(with-eval-after-load 'anzu
  (setq mode-line-position
        (list mode-line-position
              '(anzu--state " ")
              '(:eval (anzu--update-mode-line))))
  (add-to-list 'mode-line-boundary-faces 'anzu-mode-line)
  (add-to-list 'mode-line-boundary-faces 'anzu-mode-line-no-match))

(add-hook 'emacs-startup-hook #'global-anzu-mode)


(resolve init-anzu)
