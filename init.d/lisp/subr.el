
;;;; subr.el


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
;; (defmacro call-with-runtime-bindings (binders function name)
;;   (let ((defcustom-section nil))
;;     (let ((defadvice-section
;;             (mapcar
;;              (lambda (binder)
;;                (cond ((functionp (cadr binder))
;;                       (list (car binder) `(funcall ,(cadr binder))))
;;                      ((and (listp (cadr binder))
;;                            (eq 'quote (car (cadr binder)))
;;                            (functionp (cadr (cadr binder))))
;;                       (list (car binder)
;;                             `(if (fboundp ,(cadr binder))
;;                                  (funcall ,(cadr binder))
;;                                ,(car binder))))
;;                      ((and (symbolp  (cadr binder))
;;                            (not (boundp (cadr binder))))
;;                       (progn
;;                         (setq defcustom-section
;;                               (cons `(defcustom ,(cadr binder) nil "")
;;                                     defcustom-section))
;;                         (list (car binder)
;;                               `(if ,(cadr binder)
;;                                    (funcall ,(cadr binder))
;;                                  ,(car binder)))))
;;                      ((and (symbolp  (cadr binder))
;;                            (functionp (eval (cadr binder))))
;;                       (list (car binder)
;;                             `(if ,(cadr binder)
;;                                  (funcall ,(cadr binder))
;;                                ,(car binder))))
;;                      ;; Other conditions are not supposed.
;;                      ;; throw error at runtime by let binding or funcall
;;                      (t (list (car binder) `(funcall ,(cadr binder))))))
;;              binders)))
;;       `(progn
;;          ,@defcustom-section
;;          (defadvice ,function (around ,name)
;;            (let ,defadvice-section
;;              ad-do-it))
;;          (ad-activate ',function)))))
