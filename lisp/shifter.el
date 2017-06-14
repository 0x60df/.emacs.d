;;; shifter.el --- shift major/minor-mode

;;; code:

(require 'fmmm)
(require 'cl-lib)

(defgroup shifter nil
  "shift major/minor-mode"
  :group 'emacs)

(defcustom shifter-hist-file (concat user-emacs-directory "shifter-hists")
  "File which stores shifter history."
  :group 'shifter
  :type 'file)

(defcustom shifter-keep-hist-volatile nil
  "When non-nil shifter does not save/load hist file automatically."
  :group 'shifter
  :type 'boolean)

(defvar shifter-major-mode-hist nil "History of major modes shifted in past")
(defvar shifter-minor-mode-hist nil "History of minor modes shifted in past")

(defun shifter-save-hist ()
  "save shifter-major/minor-mode-hist to `shifter-hist-file'"
  (with-temp-buffer
    (insert (format "(setq shifter-major-mode-hist '%S)\n\n"
                    shifter-major-mode-hist)
            (format "(setq shifter-minor-mode-hist '%S)\n\n"
                    shifter-minor-mode-hist))
    (write-file shifter-hist-file)))

;;;###autoload
(defun shifter-shift-major-mode ()
  "shift major mode"
  (interactive)
  (if (and (not shifter-keep-hist-volatile)
           (not (bound-and-true-p shifter-non-volatile-hist-mode)))
      (shifter-non-volatile-hist-mode 1))
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
(defun shifter-shift-minor-mode ()
  "shift minor mode"
  (interactive)
  (if (and (not shifter-keep-hist-volatile)
           (not (bound-and-true-p shifter-non-volatile-hist-mode)))
      (shifter-non-volatile-hist-mode 1))
  (let ((l (fmmm-minor-mode-list))
        (h shifter-minor-mode-hist))
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
      (let ((a (if (and (boundp s) (symbol-value s))
                   -1
                 1)))
        (funcall s a)))))

;;;###autoload
(defun shifter-turn-on-minor-mode (force)
  "turn on minor mode"
  (interactive "P")
  (if (and (not shifter-keep-hist-volatile)
           (not (bound-and-true-p shifter-non-volatile-hist-mode)))
      (shifter-non-volatile-hist-mode 1))
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
  (if (and (not shifter-keep-hist-volatile)
           (not (bound-and-true-p shifter-non-volatile-hist-mode)))
      (shifter-non-volatile-hist-mode 1))
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

;;;###autoload
(define-minor-mode shifter-non-volatile-hist-mode
  "Minor mode for supporting shifter history system.
When enabled, load `shifter-hist-file' if shifter-major/minor-mode-hist are nil.
When kill emacs, save shifter-major/minor-mode-hist if this mode is enabled."
  :group 'shifter
  :global t
  (if shifter-non-volatile-hist-mode
      (progn
        (add-hook 'kill-emacs-hook #'shifter-save-hist)
        (if (and (null shifter-major-mode-hist) (null shifter-minor-mode-hist))
            (load shifter-hist-file t nil t)))
    (remove-hook 'kill-emacs-hook #'shifter-save-hist)))

(provide 'shifter)

;;; shifter.el ends here
