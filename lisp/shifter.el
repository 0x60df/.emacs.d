;;; shifter.el --- Shift major/minor-mode

;;; Commentary:

;;; Code:

(require 'seq)
(require 'fmmm)

(defgroup shifter nil
  "Shift major/minor-mode."
  :group 'emacs)

(defcustom shifter-use-hist t
  "Flag if `shifter' use history."
  :group 'shifter
  :type 'boolean)

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
        (h (if shifter-use-hist shifter-major-mode-hist)))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Major mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (letrec ((inspect
                (lambda (ms)
                  (let ((f (symbol-function ms)))
                    (cond ((null f) f)
                          ((symbolp f) (funcall inspect f))
                          (t f))))))
        (let ((f (funcall inspect s)))
          (if (autoloadp f) (autoload-do-load f))))
      (if (not (fmmm-major-mode-p s))
          (setq s 'fundamental-mode))
      (when (and shifter-use-hist (fmmm-major-mode-p s))
        (setq shifter-major-mode-hist (delq s shifter-major-mode-hist))
        (add-to-list 'shifter-major-mode-hist s))
      (funcall s))))

;;;###autoload
(defun shifter-shift-minor-mode (&optional shift force)
  "Shift minor mode.

SHIFT has same role as an argument for general minor-mode.
If SHIFT is quoted toggle, toggle minor-mode.
If SHIFT is zero or negative integer, disable minor-mode.
Otherwise, enable minor-mode.

FORCE takes effect if SHIFT is other than quoted toggle.
If FORCE is non-nil, this function lists up candidates and
perform operation according to SHIFT unconditionally.
Otherwise, counterparts will be listed up for candidates,
that is to say, disabled modes for enabling, enabled modes
for disabling."
  (interactive `(,(cond ((null current-prefix-arg) 'toggle)
                        ((< (prefix-numeric-value current-prefix-arg) 1) 0)
                        (t nil))
                 ,(if (or (and (consp current-prefix-arg)
                               (let ((num (car current-prefix-arg)))
                                 (or (< 4 num)
                                     (< num -1))))
                          (and (integerp current-prefix-arg)
                               (or (< 9 current-prefix-arg)
                                   (< current-prefix-arg 0))))
                      t)))
  (let* ((l (cond ((eq shift 'toggle) (fmmm-minor-mode-list))
                  ((and (integerp shift) (< shift 1) (not force))
                   (fmmm-enabled-minor-mode-list))
                  ((not force) (fmmm-disabled-minor-mode-list))
                  (t (fmmm-minor-mode-list))))
         (h (if shifter-use-hist
                (funcall
                 (cond ((eq shift 'toggle) #'identity)
                       ((and (integerp shift) (< shift 1) (not force))
                        (lambda (hist)
                          (let ((enabled-list (fmmm-enabled-minor-mode-list)))
                            (seq-filter (lambda (e) (memq e enabled-list))
                                        hist))))
                       ((not force)
                        (lambda (hist)
                          (let ((enabled-list (fmmm-enabled-minor-mode-list)))
                            (seq-filter (lambda (e) (not (memq e enabled-list)))
                                        hist))))
                       (t #'identity))
                 shifter-minor-mode-hist))))
    (mapc (lambda (s) (setq l (delq s l))) h)
    (let ((s (intern (completing-read "Minor mode: "
                                      (mapcar 'symbol-name (append h l))))))
      (letrec ((inspect
                (lambda (ms)
                  (let ((f (symbol-function ms)))
                    (cond ((null f) f)
                          ((symbolp f) (funcall inspect f))
                          (t f))))))
        (let ((f (funcall inspect s)))
          (if (autoloadp f) (autoload-do-load f))))
      (if (not (fmmm-minor-mode-p s))
          (setq s 'ignore))
      (when (and shifter-use-hist (fmmm-minor-mode-p s))
        (setq shifter-minor-mode-hist (delq s shifter-minor-mode-hist))
        (add-to-list 'shifter-minor-mode-hist s))
      (funcall s shift))))

;;;###autoload
(define-minor-mode shifter-non-volatile-hist-mode
  "Minor mode for supporting shifter history system.
When enabled, load `shifter-hist-file' and push current
shifter-major/minor-mode-hist on loaded history.
If this mode is enabled, history is saved when emacs is killed."
  :group 'shifter
  :global t
  (if shifter-non-volatile-hist-mode
      (when shifter-use-hist
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
