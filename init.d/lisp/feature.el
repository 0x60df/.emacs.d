
;;;; feature.el


(premise init)

(defmacro idleload (function secs)
  "`autoload-do-load' FUNCTION by idle timer which is set by SECS.

FUNCTION must be a symbol, which is embedded to lambda form
for `run-with-idle-timer'. Hence, if any other form is used,
it will be evaluated with global context when the timer is
performed. It may cause unstable behavior.
SECS must be something number, which is valid for
`run-with-idle-timer'."
  `(put ,function 'idleload
        (run-with-idle-timer
         ,secs nil (lambda ()
                     (autoload-do-load (symbol-function ,function))))))


(resolve feature)
