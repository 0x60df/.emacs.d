
;;;; init-anzu.el


(premise init)
(premise custom)
(premise advice)
(premise bindings)
(premise mode-line)
(premise inst-anzu)

(custom-set-variables
 '(anzu-cons-mode-line-p nil)
 '(anzu-mode-lighter ""))

(overriding-set-key (kbd "M-%") #'anzu-query-replace)
(overriding-set-key (kbd "C-M-%") #'anzu-query-replace-regexp)
(overriding-set-key (kbd "H-5") #'anzu-query-replace)
(overriding-set-key (kbd "H-C-5") #'anzu-query-replace-regexp)
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

(advice-add-for-once 'isearch-mode
                     :before (lambda (&rest args) (global-anzu-mode)))


(resolve init-anzu)
