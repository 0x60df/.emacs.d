
;;;; init-yester-theme.el


(premise init)
(premise inst-yester-theme)

(eval-when-compile (require 'yester))

(declare-function yester-phase-shift-mode "yester")
(declare-function yester-recalc "yester")

(declare-function yester--recalc-once-on-post-command-for-accessory
                  load-file-name t t)
(declare-function yester--background-mode-guard-for-accessory
                  load-file-name t t)

(with-eval-after-load 'yester
  (add-to-list 'yester-recalc 'yester-accessory)
  (yester-phase-shift-mode))


(resolve init-yester-theme)
