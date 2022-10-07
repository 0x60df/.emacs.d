
;;;; init-orderless.el


(premise init)
(premise custom)
(premise inst-orderless)

(setq completion-category-defaults nil)

(defun orderless-without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern) '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(custom-set-variables
 '(completion-styles '(orderless basic))
 '(completion-category-overrides)
 '(orderless-style-dispatchers '(orderless-without-if-bang)))


(resolve init-orderless)
