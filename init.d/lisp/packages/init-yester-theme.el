
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
  (defun yester--recalc-once-on-post-command-for-accessory ()
    "`yester--recalc-once-on-post-command' but for accessory theme."
    (yester-recalc 'yester-accessory)
    (remove-hook 'post-command-hook
                 #'yester--recalc-once-on-post-command-for-accessory))

  (defun yester--background-mode-guard-for-accessory
      (symbol newval operation where)
    "`yester--background-mode-guard' but for accessory theme."
    (if (eq operation 'set)
        (add-hook 'post-command-hook
                  #'yester--recalc-once-on-post-command-for-accessory)))

  (add-hook 'yester-phase-shift-mode-hook
            (lambda ()
              (if yester-phase-shift-mode
                  (add-variable-watcher
                   'frame-background-mode
                   #'yester--background-mode-guard-for-accessory)
                (remove-variable-watcher
                 'frame-background-mode
                 #'yester--background-mode-guard-for-accessory))))

    (yester-phase-shift-mode))


(resolve init-yester-theme)
