
;;;; init-orderless.el


(premise init)
(premise custom)
(premise inst-orderless)

(setq completion-category-defaults nil)

(custom-set-variables
 '(completion-styles '(orderless basic))
 '(completion-category-overrides))


(resolve init-orderless)
