
;;;; init-hi-lock.el


(premise init)
(premise bindings)

(overriding-set-key (kbd "C-l h l m") #'hi-lock-mode)
(overriding-set-key (kbd "C-l h l b") #'hi-lock-face-buffer)
(overriding-set-key (kbd "C-l h l u") #'hi-lock-unface-buffer)
(overriding-set-key (kbd "C-l h l r") #'hi-lock-face-phrase-buffer)
(overriding-set-key (kbd "C-l h l l") #'hi-lock-line-face-buffer)
(overriding-set-key (kbd "C-l h l s") #'hi-lock-face-symbol-at-point)


(resolve init-hi-lock)
