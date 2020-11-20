
;;;; init-ruby-end.el


(premise init)
(premise mode-line)
(premise inst-ruby-end)

(push '(ruby-end-mode . 1) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'ruby-end
  (setcdr (assq 'ruby-end-mode minor-mode-alist) '(" en")))


(resolve init-ruby-end)
