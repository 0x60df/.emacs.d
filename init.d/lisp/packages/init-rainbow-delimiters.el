
;;;; init-rainbow-delimiters.el


(premise init)
(premise inst-rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(resolve init-rainbow-delimiters)
