
;;;; subr.el


(premise init)
(premise print)

(defun echo (&rest args)
  "Message ARGS delimited by space."
  (apply #'message (mapconcat (lambda (a) [?% ?s]) args " ") args))

(defun add-hook-for-once (hook function &optional depth local trigger)
  "Add to the value of HOOK the function FUNCTION for once use.

Hooked function is not a FUNCTION but uninterned symbol `:',
which stores lambda form containing FUNCTION and
`remove-hook' form for #::.
DEPTH and LOCAL are passed to `add-hook' and `remove-hook'.
When TRIGGER is non-nil, running FUNCTION and `remove-hook'
is delayed until funcall TRIGGER returns non-nil."
  (let ((symbol (make-symbol ":")))
    (fset symbol `(lambda (&rest args)
                    (if (or ,(null trigger) (funcall ',trigger))
                        (unwind-protect
                            (apply ',function args)
                          (remove-hook ',hook ',symbol ',local)))))
    (add-hook hook symbol depth local)))

(defun remove-hook-for-once (hook function &optional local trigger)
  "Remove from the value of HOOK the function whose body is FUNCTION.

Optional argument LOCAL is passed to `remove-hook'.

When optional argument TRIGGER is non-nil, `remove-hook' is
performed only if #:: is added with trigger function which
equals to TRIGGER.
On the other hand, #:: with trigger can be removed only if
TRIGGER equals to the trigger."
  (let ((symbol (seq-find
                 (lambda (f)
                   (and (symbolp f)
                        (equal (symbol-name f) ":")
                        (not (eq f (intern ":")))
                        (if trigger
                            (equal (cadr (caddr (cadr (caddr
                                                       (symbol-function f)))))
                                   `',trigger)
                          (cadr (cadr (caddr (symbol-function f)))))
                        (equal (cadr (cadr (caddr (caddr (symbol-function f)))))
                               `',function)))
                 (symbol-value hook))))
    (remove-hook hook symbol local)))


(resolve subr)
