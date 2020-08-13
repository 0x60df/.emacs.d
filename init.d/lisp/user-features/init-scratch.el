
;;;; init-scratch.el


(premise init)

(global-set-key (kbd "C-c b") 'scratch)
(global-set-key (kbd "C-l b") 'scratch-shred)
(define-key scratch-mode-map (kbd "ESC ESC DEL") #'scratch-kill-current-buffer)
(define-key scratch-mode-map (kbd "ESC ESC RET") #'scratch-label)


(resolve scratch)
