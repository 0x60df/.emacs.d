
;;;; init-server.el


(when (daemonp)
  (add-hook 'server-visit-hook
            (lambda () (keyboard-translate ?\C-h ?\C-?)))

  (defun server-after-first-make-frame-function (frame)
    (select-frame frame)
    (remove-hook 'after-make-frame-functions
                 'server-after-first-make-frame-function))

  (add-hook 'after-init-hook
            (lambda () (ad-activate 'server-after-first-make-frame-function)))

  (add-hook 'after-make-frame-functions
            'server-after-first-make-frame-function))

(defmacro server-eval-after-first-make-frame (name &rest body)
  `(cond ((daemonp)
          (defadvice server-after-first-make-frame-function (after ,name)
            ,@body))
         (t ,@body)))
