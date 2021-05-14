
;;;; init-frame-dealer.el


(premise init)
(premise mode-line)
(premise inst-frame-dealer)

(push '(frame-dealer-mode . 15) mode-line-minor-mode-priority-alist)


(resolve init-frame-dealer)
