
;;;; subr.el


(premise init)

(defun echo (&rest args)
  "Message ARGS delimited by space."
  (apply #'message (mapconcat (lambda (a) [?% ?s]) args " ") args))


(resolve subr)
