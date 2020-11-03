
;;;; init-ruby-end.el


(premise init)
(premise inst-ruby-end)

(with-eval-after-load 'ruby-end
  (setcdr (assq 'ruby-end-mode minor-mode-alist) '(" en")))


(resolve init-ruby-end)
