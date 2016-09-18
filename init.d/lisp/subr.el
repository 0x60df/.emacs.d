
;;;; subr.el


(defmacro call-with-runtime-bindings (binders function name)
  (let ((defcustom-section nil))
    (let ((defadvice-section
            (mapcar
             (lambda (binder)
               (cond ((functionp (cadr binder))
                      (list (car binder) `(funcall ,(cadr binder))))
                     ((and (listp (cadr binder))
                           (eq 'quote (car (cadr binder)))
                           (functionp (cadr (cadr binder))))
                      (list (car binder)
                            `(if (fboundp ,(cadr binder))
                                 (funcall ,(cadr binder))
                               ,(car binder))))
                     ((and (symbolp  (cadr binder))
                           (not (boundp (cadr binder))))
                      (progn
                        (setq defcustom-section
                              (cons `(defcustom ,(cadr binder) nil "")
                                    defcustom-section))
                        (list (car binder)
                              `(if ,(cadr binder)
                                   (funcall ,(cadr binder))
                                 ,(car binder)))))
                     ((and (symbolp  (cadr binder))
                           (functionp (eval (cadr binder))))
                      (list (car binder)
                            `(if ,(cadr binder)
                                 (funcall ,(cadr binder))
                               ,(car binder))))
                     ;; Other conditions are not supposed.
                     ;; throw error at runtime by let binding or funcall
                     (t (list (car binder) `(funcall ,(cadr binder))))))
             binders)))
      `(progn
         ,@defcustom-section
         (defadvice ,function (around ,name)
           (let ,defadvice-section
             ad-do-it))
         (ad-activate ',function)))))
