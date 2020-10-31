
;;;; init-expand-region.el


(premise init)
(premise bindings)
(premise inst-expand-region)

(declare-function er/contract-region "expand-region")

(overriding-set-key (kbd "C-`") #'er/expand-region)
(with-eval-after-load 'expand-region
  (overriding-set-key (kbd "C-M-`") #'er/contract-region))


(resolve init-expand-region)
