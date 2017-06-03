;;; shifter.el --- shift major/minor-mode

;;; code:

(require 'fmmm)

(defgroup shifter nil
  "shift major/minor-mode"
  :group 'emacs)

;;;###autoload
(defun shifter-shift-major-mode ()
  "shift major mode"
  (interactive)
  (let ((s (intern
            (completing-read
             "Major mode: "
             (mapcar 'symbol-name (fmmm-major-mode-list))))))
    (letrec ((trace-to-function
              (lambda (ms)
                (let ((f (symbol-function ms)))
                  (cond ((autoloadp f) f)
                        ((symbolp f)
                         (funcall trace-to-function f))
                        (t f))))))
      (let ((f (funcall trace-to-function s)))
        (if (autoloadp f) (autoload-do-load f))))
    (if (not (fmmm-major-mode-p s))
        (setq s 'fundamental-mode))
    (funcall s)))

;;;###autoload
(defun shifter-turn-on-minor-mode (force)
  "turn on minor mode"
  (interactive "P")
  (let ((s (intern
            (completing-read
             "Minor mode: "
             (mapcar 'symbol-name
                     (if force
                         (fmmm-minor-mode-list)
                       (fmmm-disbled-minor-mode-list)))))))
    (let ((f (symbol-function s)))
      (if (autoloadp f) (autoload-do-load f)))
    (if (not (fmmm-minor-mode-p s))
        (setq s 'ignore))
    (funcall s 1)))

;;;###autoload
(defun shifter-turn-off-minor-mode (force)
  "turn off minor mode"
  (interactive "P")
  (let ((s (intern
            (completing-read
             "Minor mode: "
             (mapcar 'symbol-name
                     (if force
                         (fmmm-minor-mode-list)
                       (fmmm-enabled-minor-mode-list)))))))
    (let ((f (symbol-function s)))
      (if (autoloadp f) (autoload-do-load f)))
    (if (not (fmmm-minor-mode-p s))
        (setq s 'ignore))
    (funcall s -1)))

(provide 'shifter)

;;; shifter.el ends here
