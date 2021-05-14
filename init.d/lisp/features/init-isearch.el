
;;;; init-isearch.el


(premise init)
(premise mode-line)

(push '(isearch-mode . 20) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'isearch
  (modify-minor-mode-lighter 'isearch-mode '(-3 "" isearch-mode)))


(resolve init-isearch)
