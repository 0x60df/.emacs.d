
;;;; subr.el


(premise init)

(defun echo (&rest args)
  "Message ARGS delimited by space."
  (apply #'message (mapconcat (lambda (a) "%s" [?% ?s]) args " ") args))

(defmacro call-with-runtime-bindings (binders function name)
  (let ((varlist
          (mapcar
           (lambda (binder)
             (cond ((functionp (cadr binder)) ;lambda
                    (list (car binder) `(funcall ,(cadr binder))))
                   ((and (listp (cadr binder)) ;function
                         (eq 'function (car (cadr binder))))
                    (list (car binder) `(funcall ,(cadr (cadr binder)))))
                   ((and (listp (cadr binder))
                         (eq 'quote (car (cadr binder))))
                    (list (car binder)
                          `(if (functionp ,(cadr binder))
                               (funcall ,(cadr binder))
                             (if (boundp ',(car binder))
                                 ,(car binder)
                               nil)))) ;quoted exp i.e. function-name or lambda
                   ((symbolp (cadr binder))
                    (list (car binder)
                          `(if (and (boundp ',(cadr binder))
                                    (functionp ,(cadr binder)))
                               (funcall ,(cadr binder))
                             (if (boundp ',(car binder))
                                 ,(car binder)
                               nil)))) ;symbol i.e. variable-name
                   ;; Other conditions are not supposed.
                   ;; bind original value
                   (t (list (car binder) `(if (boundp ',(car binder))
                                              ,(car binder)
                                            nil)))))
           binders)))
    `(progn
       (defadvice ,function (around ,name)
         (let ,varlist
           ad-do-it))
       (ad-activate ',function))))


(resolve subr)
