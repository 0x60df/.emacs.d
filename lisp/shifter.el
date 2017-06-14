;;; shifter.el --- shift major/minor-mode

;;; code:

(require 'fmmm)
(require 'cl-lib)

(defgroup shifter nil
  "shift major/minor-mode"
  :group 'emacs)

(defvar shifter-major-mode-hist nil "History of major modes shifted in past")
(defvar shifter-minor-mode-hist nil "History of minor modes shifted in past")

;;;###autoload
(defun shifter-shift-major-mode ()
  "shift major mode"
  (interactive)
  (let ((l (fmmm-major-mode-list))
        (h shifter-major-mode-hist))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Major mode: "
                                      (mapcar 'symbol-name (append h l))))))
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
      (when (fmmm-major-mode-p s)
        (delq s shifter-major-mode-hist)
        (add-to-list 'shifter-major-mode-hist s))
      (funcall s))))

;;;###autoload
(defun shifter-turn-on-minor-mode (force)
  "turn on minor mode"
  (interactive "P")
  (let ((l (if force
               (fmmm-minor-mode-list)
             (fmmm-disbled-minor-mode-list)))
        (h (funcall (if force
                        'identity
                      (lambda (l)
                        (cl-remove-if
                         (lambda (s) (memq s (fmmm-enabled-minor-mode-list)))
                         l)))
                    shifter-minor-mode-hist)))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Minor mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (let ((f (symbol-function s)))
        (if (autoloadp f) (autoload-do-load f)))
      (if (not (fmmm-minor-mode-p s))
          (setq s 'ignore))
      (when (fmmm-minor-mode-p s)
        (delq s shifter-minor-mode-hist)
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s 1))))

;;;###autoload
(defun shifter-turn-off-minor-mode (force)
  "turn off minor mode"
  (interactive "P")
  (let ((l (if force
               (fmmm-minor-mode-list)
             (fmmm-enabled-minor-mode-list)))
        (h (funcall (if force
                        'identity
                      (lambda (l)
                        (cl-remove-if
                         (lambda (s) (memq s (fmmm-disbled-minor-mode-list)))
                         l)))
                    shifter-minor-mode-hist)))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Minor mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (let ((f (symbol-function s)))
        (if (autoloadp f) (autoload-do-load f)))
      (if (not (fmmm-minor-mode-p s))
          (setq s 'ignore))
      (when (fmmm-minor-mode-p s)
        (delq s shifter-minor-mode-hist)
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s -1))))

(provide 'shifter)

;;; shifter.el ends here
