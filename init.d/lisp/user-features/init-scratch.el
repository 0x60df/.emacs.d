
;;;; init-scratch.el

(premise init)

(scratch-declare-major-mode 'ruby-mode)
(global-set-key (kbd "C-c b") 'scratch)


(resolve scratch)
