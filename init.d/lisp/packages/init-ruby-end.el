
;;;; init-ruby-end.el


(premise init)
(premise mode-line)
(premise inst-ruby-end)

(push '(ruby-end-mode . 1) mode-line-minor-mode-priority-alist)


(resolve init-ruby-end)
