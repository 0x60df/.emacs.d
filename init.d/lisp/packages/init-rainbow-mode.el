
;;;; init-rainbow-mode.el


(premise init)
(premise mode-line)
(premise inst-rainbow-mode)

(push '(rainbow-mode . 45) mode-line-minor-mode-priority-alist)


(resolve init-rainbow-mode)
