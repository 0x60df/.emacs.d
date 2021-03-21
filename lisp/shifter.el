;;; shifter.el --- shift major/minor-mode

;;; Commentary:

;;; Code:

(require 'fmmm)
(require 'cl-lib)

(defgroup shifter nil
  "Shift major/minor-mode."
  :group 'emacs)

(defcustom shifter-hist-file (concat user-emacs-directory "shifter-hists")
  "File which stores shifter history."
  :group 'shifter
  :type 'file)

(defvar shifter-major-mode-hist nil "History of major modes shifted in past.")
(defvar shifter-minor-mode-hist nil "History of minor modes shifted in past.")

(defun shifter-save-hist ()
  "Save shifter-major/minor-mode-hist to `shifter-hist-file'."
  (with-temp-buffer
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m shifter-major-mode-hist)))
                  ',(reverse shifter-major-mode-hist))
           (current-buffer))
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m shifter-minor-mode-hist)))
                  ',(reverse shifter-minor-mode-hist))
           (current-buffer))
    (write-file shifter-hist-file)))

;;;###autoload
(defun shifter-shift-major-mode ()
  "Shift major mode."
  (interactive)
  (let ((l (fmmm-major-mode-list))
        (h shifter-major-mode-hist))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Major mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (letrec ((trace-to-function
                (lambda (ms)
                  (let ((f (symbol-function ms)))
                    (cond ((null f) f)
                          ((symbolp f) (funcall trace-to-function f))
                          (t f))))))
        (let ((f (funcall trace-to-function s)))
          (if (autoloadp f) (autoload-do-load f))))
      (if (not (fmmm-major-mode-p s))
          (setq s 'fundamental-mode))
      (when (fmmm-major-mode-p s)
        (setq shifter-major-mode-hist (delq s shifter-major-mode-hist))
        (add-to-list 'shifter-major-mode-hist s))
      (funcall s))))

;;;###autoload
(defun shifter-shift-minor-mode ()
  "Shift minor mode."
  (interactive)
  (let ((l (fmmm-minor-mode-list))
        (h shifter-minor-mode-hist))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Minor mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (letrec ((trace-to-function
                (lambda (ms)
                  (let ((f (symbol-function ms)))
                    (cond ((null f) f)
                          ((symbolp f) (funcall trace-to-function f))
                          (t f))))))
        (let ((f (funcall trace-to-function s)))
          (if (autoloadp f) (autoload-do-load f))))
      (if (not (fmmm-minor-mode-p s))
          (setq s 'ignore))
      (when (fmmm-minor-mode-p s)
        (setq shifter-minor-mode-hist (delq s shifter-minor-mode-hist))
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s 'toggle))))

;;;###autoload
(defun shifter-turn-on-minor-mode (force)
  "Turn on minor mode.
If FORCE is non-nil, read and enable minor mode regardless
of its state."
  (interactive "P")
  (let ((l (if force
               (fmmm-minor-mode-list)
             (fmmm-disabled-minor-mode-list)))
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
        (setq shifter-minor-mode-hist (delq s shifter-minor-mode-hist))
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s 1))))

;;;###autoload
(defun shifter-turn-off-minor-mode (force)
  "Turn off minor mode.
If FORCE is non-nil, read and disable minor mode regardless
of its state."
  (interactive "P")
  (let ((l (if force
               (fmmm-minor-mode-list)
             (fmmm-enabled-minor-mode-list)))
        (h (funcall (if force
                        'identity
                      (lambda (l)
                        (cl-remove-if
                         (lambda (s) (memq s (fmmm-disabled-minor-mode-list)))
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
        (setq shifter-minor-mode-hist (delq s shifter-minor-mode-hist))
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s -1))))

;;;###autoload
(define-minor-mode shifter-non-volatile-hist-mode
  "Minor mode for supporting shifter history system.
When enabled, load `shifter-hist-file' and push current
shifter-major/minor-mode-hist on loaded history.
If this mode is enabled, history is saved when emacs is killed."
  :group 'shifter
  :global t
  (if shifter-non-volatile-hist-mode
      (progn
        (add-hook 'kill-emacs-hook #'shifter-save-hist)
        (let ((major-mode-hist shifter-major-mode-hist)
              (minor-mode-hist shifter-minor-mode-hist))
          (load shifter-hist-file t nil t)
          (mapc (lambda (m)
                  (setq shifter-major-mode-hist
                        (delq m shifter-major-mode-hist)))
                major-mode-hist)
          (setq shifter-major-mode-hist
                (append major-mode-hist shifter-major-mode-hist))
          (mapc (lambda (m)
                  (setq shifter-minor-mode-hist
                        (delq m shifter-minor-mode-hist)))
                minor-mode-hist)
          (setq shifter-minor-mode-hist
                (append minor-mode-hist shifter-minor-mode-hist))
          (if (or major-mode-hist minor-mode-hist)
              (shifter-save-hist))))
    (remove-hook 'kill-emacs-hook #'shifter-save-hist)))

(provide 'shifter)

;;; shifter.el ends here
