
;;;; init-anzu.el


(premise init)
(premise inst-anzu)

(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(define-key isearch-mode-map [remap isearch-query-replace]
  'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp]
  'anzu-isearch-query-replace-regexp)

(defvar mode-line-anzu nil)

(eval-after-load 'anzu
  '(progn
     (custom-set-variables
      '(anzu-cons-mode-line-p nil)
      '(anzu-mode-lighter " A"))

     (setq mode-line-position (list mode-line-position
                                    " "
                                    '(:eval (anzu--update-mode-line))))))


(resolve init-anzu)
