
;;;; init-web-mode.el


(premise init)
(premise inst-web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(resolve init-web-mode)
