
;;;; init-ag.el



;;; base

(eval-after-load 'ag
  '(custom-set-variables '(ag-group-matches nil)))


;;; bindings

(global-set-key "\C-csa" 'ag)
(global-set-key "\M-sa" 'ag)
