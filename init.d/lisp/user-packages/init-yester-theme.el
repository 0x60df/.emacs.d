
;;;; init-yester-theme.el


(premise init)
(premise inst-yester-theme)

(declare-function yester-phase-shift-mode "yester")

(with-eval-after-load 'yester
  (yester-phase-shift-mode))


(resolve init-yester-theme)
