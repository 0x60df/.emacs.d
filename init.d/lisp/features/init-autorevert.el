
;;;; init-autorevert.el


(premise init)
(premise custom)
(premise mode-line)

(custom-set-variables
 '(auto-revert-mode-text " ARv"))

(push '(auto-revert-mode . 32) mode-line-minor-mode-priority-alist)


(resolve init-autorevert)
