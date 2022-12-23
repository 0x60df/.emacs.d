
;;;; init-expand-region.el


(premise init)
(premise bindings)
(premise inst-expand-region)

(declare-function er/contract-region "expand-region")

(custom-set-variables
 '(expand-region-contract-fast-key "`"))

(overriding-set-key (kbd "C-`") #'er/expand-region)
(overriding-set-key (kbd "M-@") #'er/expand-region)
(with-eval-after-load 'expand-region
  (overriding-set-key (kbd "C-M-`") #'er/contract-region))

(add-to-list 'balance-mode-key-list (kbd "C-`"))


(resolve init-expand-region)
