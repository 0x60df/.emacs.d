
;;;; init-wgrep.el


(premise init)
(premise custom)
(premise inst-wgrep)

(custom-set-variables
 '(wgrep-auto-save-buffer t)
 '(wgrep-enable-key "r"))


(resolve init-wgrep)
