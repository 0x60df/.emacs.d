
;;;; advice.el


(premise init)
(premise print)

(defun advice-add-for-once (symbol where function &optional props trigger)
  "`advice-add' for one time use.

Advising function is not a FUNCTION but uninterned symbol
`:', whick stores lambda form containing FUNCTION and
`advice-remove' form for #::.
SYMBOL, WHERE, and PROPS are passed to `advice-add' and
`advice-remove'.
When TRIGGER is non-nil, running FUNCTION and
`advice-remove' is delayed until funcall TRIGGER returns
non-nil."
  (let ((advice (make-symbol ":")))
    (fset advice `(lambda (&rest args)
                    (if (or ,(null trigger) (funcall ',trigger))
                        (unwind-protect
                            (apply ',function args)
                          (advice-remove ',symbol ',advice)))))
    (advice-add symbol where advice props)))

(defun advice-remove-for-once (symbol function &optional trigger)
  "`advice-remove' for the function whose body is FUNCTION.
Find the function for the SYMBOL, and `advice-remove' it.

When optional argument TRIGGER is non-nil, `advice-remove'
is performed only if #:: is added with trigger function
which equals to TRIGGER.
On the other hand, #:: with trigger can be removed only if
TRIGGER equals to the trigger."
  (let (advice)
    (advice-mapc (lambda (f p)
                   (if (and (symbolp f)
                            (equal (symbol-name f) ":")
                            (not (eq f (intern ":")))
                            (if trigger
                                (equal (cadr (caddr (cadr
                                                     (caddr
                                                      (symbol-function f)))))
                                       `',trigger)
                              (cadr (cadr (caddr (symbol-function f)))))
                            (equal (cadr (cadr (caddr (caddr
                                                       (symbol-function f)))))
                                   `',function))
                       (setq advice f)))
                 symbol)
    (if advice                          ; test advice to avoid remove all, BUG?
        (advice-remove symbol advice))))


(resolve advice)
