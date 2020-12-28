
;;;; subr.el


(premise init)
(premise print)

(defun echo (&rest args)
  "Message ARGS delimited by space."
  (apply #'message (mapconcat (lambda (a) [?% ?s]) args " ") args))

(defun add-hook-for-once (hook function &optional depth local)
  "Add to the value of HOOK the function FUNCTION for once use.

Hooked function is not a FUNCTION but uninterned symbol,
`transient-hook-function', which stores lambda form
containing FUNCTION and `remove-hook' for
#:transient-hook-function.
DEPTH and LOCAL are passed to `add-hook' and `remove-hook'."
  (let ((symbol (make-symbol "transient-hook-function")))
    (fset symbol `(lambda ()
                    (unwind-protect
                        (funcall ',function)
                      (remove-hook ',hook ',symbol ',local))))
    (add-hook hook symbol depth local)))

(defun remove-hook-for-once (hook function &optional local)
  "Remove from the value of HOOK the function whose body is FUNCTION."
  (let ((symbol (seq-find
                 (lambda (f)
                   (and (symbolp f)
                        (equal (symbol-name f) "transient-hook-function")
                        (not (eq f (intern "transient-hook-function")))
                        (equal (cadr (cadr (caddr (symbol-function f))))
                               `',function)))
                          (symbol-value hook))))
    (remove-hook hook symbol local)))


(resolve subr)
