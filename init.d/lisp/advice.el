
;;;; advice.el


(premise init)
(premise print)

(defun advice-add-for-once (symbol where function &optional props)
  "`advice-add' for one time use.

Advising function is not a FUNCTION but uninterned symbol,
`transient-advice-function', whick stores lambda form
containing FUNCTION and `advice-remove' for
#:transient-advice-function.
SYMBOL, WHERE, and PROPS are passed to `advice-add' and
`advice-remove'."
  (let ((advice (make-symbol "transient-advice-function")))
    (fset advice `(lambda (&rest args)
                    (unwind-protect
                        (apply ',function args)
                      (advice-remove ',symbol ',advice))))
    (advice-add symbol where advice props)))

(defun advice-remove-for-once (symbol function)
  "`advice-remove' for the function whose body is FUNCTION.
Find the function for the SYMBOL, and `advice-remove' it."
  (let (advice)
    (advice-mapc (lambda (f p)
                   (if (and (symbolp f)
                            (equal (symbol-name f) "transient-advice-function")
                            (not (eq f (intern "transient-advice-function")))
                            (equal (cadr (cadr (caddr (symbol-function f))))
                                   `',function))
                       (setq advice f)))
                 symbol)
    (advice-remove symbol advice)))


(resolve advice)
