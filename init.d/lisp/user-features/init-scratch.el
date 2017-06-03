
;;;; init-scratch.el

(premise init)

(global-set-key (kbd "C-c b") 'scratch)
(define-key scratch-mode-map (kbd "ESC ESC DEL") #'scratch-kill-current-buffer)


(resolve scratch)
