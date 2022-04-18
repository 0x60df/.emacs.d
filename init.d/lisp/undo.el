
;;;; undo.el


(premise init)
(premise subr)

(defvar-local undo-turning-point-list nil
  "List of cons cells at which undo sequence is started.")

(defvar undo-standard-cursor-color nil
  "Temporary store for standard cursor color.")

(defcustom undo-turning-point-cursor-color (face-attribute 'cursor :background)
  "Cursor color for turning point of `undo'."
  :group 'user
  :type 'color)

(advice-add 'undo
            :before (lambda (&rest _)
                      (unless (eq last-command 'undo)
                        (let ((turning-point (cdr-safe buffer-undo-list)))
                          (when (consp turning-point)
                            (push turning-point undo-turning-point-list))))))

(advice-add 'undo
            :after (lambda (&rest _)
                     (if (memq pending-undo-list undo-turning-point-list)
                         (progn
                           (setq undo-standard-cursor-color
                                 (frame-parameter nil 'cursor-color))
                           (add-hook-for-once
                            'pre-command-hook
                            (lambda ()
                              (unless (or (null undo-standard-cursor-color)
                                          (eq this-command 'undo))
                                (set-frame-parameter nil 'cursor-color
                                                     undo-standard-cursor-color)
                                (setq undo-standard-cursor-color nil))))
                           (set-frame-parameter
                            nil 'cursor-color undo-turning-point-cursor-color)
                           (when (consp buffer-undo-list)
                             (push buffer-undo-list undo-turning-point-list)
                             (setq undo-turning-point-list
                                   (seq-filter
                                    (lambda (turning-point)
                                      (and (consp buffer-undo-list)
                                           (let ((rest buffer-undo-list))
                                             (while (and rest
                                                         (not (eq turning-point
                                                                  rest)))
                                               (setq rest (cdr rest)))
                                             rest)))
                                    undo-turning-point-list))))
                       (when undo-standard-cursor-color
                         (set-frame-parameter nil 'cursor-color
                                              undo-standard-cursor-color)
                         (setq undo-standard-cursor-color nil)))))


(resolve undo)
