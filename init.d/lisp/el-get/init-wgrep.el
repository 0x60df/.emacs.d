
;;;; init-wgrep.el


(premise init)
(premise inst-wgrep)

(eval-after-load 'wgrep
  '(custom-set-variables '(wgrep-auto-save-buffer t)
                         '(wgrep-enable-key "e")))


(resolve init-wgrep)
