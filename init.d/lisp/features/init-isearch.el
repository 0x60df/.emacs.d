
;;;; init-isearch.el


(premise init)
(premise mode-line)

(push '(isearch-mode . 21) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'isearch
  (setcar (cdr (assq 'isearch-mode minor-mode-alist)) '(-3 "" isearch-mode)))


(resolve init-isearch)
