
;;;; feature.el


(premise init)

(defmacro idleload (function secs)
  "`autoload-do-load' FUNCTION by idle timer which is set by SECS.

FUNCTION must be a quoted symbol, which is embedded into
lambda form for `run-with-idle-timer'. Hence, if any other
form is used, it will be evaluated with global context when
the timer is performed. It may cause unstable behavior.
SECS must be something number, which is valid for
`run-with-idle-timer'."
  `(put ,function 'idleload
        (run-with-idle-timer
         ,secs nil (lambda ()
                     (autoload-do-load (symbol-function ,function))))))

(defmacro lazy-autoload (function file)
  "Define `autoload' for FUNCTION and FILE automatically.

FUNCTION is a quoted symbol to define autoload.
FILE is a file name string for autoload.
Rest optional arguments for autoload will be generated."
  (load file nil t)
  (let ((expand-quote (cadr function)))
    `(autoload ,function ,file
       ,(if (fboundp expand-quote) (documentation  expand-quote))
       ,(commandp expand-quote)
       ,(cond ((keymapp expand-quote) 'keymap)
              ((macrop expand-quote) 'macro)))))


(resolve feature)
