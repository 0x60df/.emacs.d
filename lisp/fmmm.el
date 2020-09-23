;;; fmmm.el --- functionalities for major/minor-mode

;;; code:

(defgroup fmmm nil
  "functionalities for major/minor-mode"
  :group 'emacs)

(defcustom fmmm-complementary-major-mode-list nil
  "List of simbols which are considered as major-mode in `fmmm'"
  :type '(list symbol)
  :group 'fmmm)

(defcustom fmmm-complementary-minor-mode-list nil
  "List of simbols which are considered as minor-mode in `fmmm'"
  :type '(list symbol)
  :group 'fmmm)

(defcustom fmmm-cache-file (concat user-emacs-directory "fmmm-cache")
  "File which stores fmmm cache."
  :group 'fmmm
  :type 'file)

(defvar fmmm-minor-mode-variable-alist
  (let (l)
    (mapatoms (lambda (a)
                (if (symbolp a)
                    (let ((minor-mode-function (get a :minor-mode-function)))
                      (if minor-mode-function
                          (setq l (cons (cons minor-mode-function a) l)))))))
    l)
  "Alist of minor-mode and minor-mode variable")

(defvar fmmm-major-mode-on-autoload-list nil
  "List of simbols of major-mode which will be autoloaded")

(defvar fmmm-minor-mode-on-autoload-list nil
  "List of simbols of minor-mode which will be autoloaded")

(defun fmmm-save-cache ()
  "Save fmmm-major/minor-mode-on-autoload-list to `fmmm-cache-file'"
  (with-temp-buffer
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m fmmm-major-mode-on-autoload-list)))
                  ',fmmm-major-mode-on-autoload-list)
           (current-buffer))
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m fmmm-minor-mode-on-autoload-list)))
                  ',fmmm-minor-mode-on-autoload-list)
           (current-buffer))
  (write-file fmmm-cache-file)))

;;;###autoload
(defun fmmm-major-mode-p (symbol)
  "Non-nil if SYMBOL seems to be major mode."
  (letrec ((inspect
            (lambda (s)
              (let ((f (symbol-function s)))
                (cond ((null f) nil)
                      ((autoloadp f) nil)
                      ((byte-code-function-p f)
                       (memq 'run-mode-hooks (append (aref f 2) nil)))
                      ((symbolp f) (funcall inspect f))
                      ((functionp f)
                       (or (get s 'derived-mode-parent)
                           (and (listp f)
                                (memq 'run-mode-hooks
                                      (mapcar
                                       (lambda (e) (if (listp e) (car e) nil))
                                       f)))))
                      (t nil))))))
    (funcall inspect symbol)))

;;;###autoload
(defun fmmm-major-mode-list ()
  "Return list consist of major mode symbol."
  (let ((valid-complemntary-list
         (letrec ((filter
                   (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l)))))))
                  (inspect
                   (lambda (s)
                     (let ((f (symbol-function s)))
                       (cond ((null f) nil)
                             ((symbolp f) (funcall inspect f))
                             (t f))))))
           (funcall filter
                    (lambda (symbol)
                      (and (not (autoloadp (funcall inspect symbol)))
                           (not (fmmm-major-mode-p symbol))))
                    fmmm-complementary-major-mode-list))))
    (let (l)
      (mapatoms
       (lambda (a) (if (and (fmmm-major-mode-p a)
                            (not (memq a valid-complemntary-list))
                            (not (memq a fmmm-major-mode-on-autoload-list)))
                       (setq l (cons a l)))))
      (append valid-complemntary-list fmmm-major-mode-on-autoload-list l))))

;;;###autoload
(defun fmmm-minor-mode-p (symbol)
  "Non-nil if SYMBOL seems to be minor mode."
  (or (memq symbol minor-mode-list)
      (assq symbol minor-mode-alist)))

;;;###autoload
(defun fmmm-minor-mode-list ()
  "Return list consist of minor mode symbol."
  (let ((valid-complemntary-list
         (letrec ((filter
                   (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l)))))))
                  (inspect
                   (lambda (s)
                     (let ((f (symbol-function s)))
                       (cond ((null f) nil)
                             ((symbolp f) (funcall inspect f))
                             (t f))))))
           (funcall filter
                    (lambda (symbol)
                      (and (not (autoloadp (funcall inspect symbol)))
                           (not (fmmm-minor-mode-p symbol))))
                    fmmm-complementary-minor-mode-list))))
    (let ((l minor-mode-list))
      (mapc (lambda (s) (setq l (delq s l))) fmmm-complementary-minor-mode-list)
      (mapc (lambda (s) (setq l (delq s l))) fmmm-minor-mode-on-autoload-list)
      (append valid-complemntary-list fmmm-minor-mode-on-autoload-list l))))

;;;###autoload
(defun fmmm-enabled-minor-mode-list ()
  "Return list consist of enabled minor mode symbol."
  (fmmm-update-minor-mode-variable-alist)
  (letrec ((filter (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l))))))))
    (funcall filter (lambda (s)
                      (let* ((cell (assq s fmmm-minor-mode-variable-alist))
                             (state (if cell (cdr cell) s)))
                        (not (and (boundp state)
                                  (symbol-value state)))))
             (fmmm-minor-mode-list))))

;;;###autoload
(defun fmmm-disabled-minor-mode-list ()
  "Return list consist of disabled minor mode symbol."
  (fmmm-update-minor-mode-variable-alist)
  (letrec ((filter (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l))))))))
    (funcall filter (lambda (s)
                      (let* ((cell (assq s fmmm-minor-mode-variable-alist))
                             (state (if cell (cdr cell) s)))
                        (and (boundp state)
                             (symbol-value state))))
             (fmmm-minor-mode-list))))

(defun fmmm-update-minor-mode-variable-alist ()
  "Update `fmmm-minor-mode-variable-alist'.
according to current `obarray'"
  (let (l)
    (mapatoms (lambda (a)
                (if (symbolp a)
                    (let ((minor-mode-function (get a :minor-mode-function)))
                      (if minor-mode-function
                          (setq l (cons (cons minor-mode-function a) l)))))))
    (setq fmmm-minor-mode-variable-alist l)))

(defconst fmmm-initial-major-mode-list (fmmm-major-mode-list)
  "List of simbols of major-mode which are loaded
at the time this feature is loaded.")

(defconst fmmm-initial-minor-mode-list (fmmm-minor-mode-list)
  "List of simbols of minor-mode which are loaded
at the time this feature is loaded.")

(defun fmmm-update-major-mode-on-autoload-list ()
  "Update `fmmm-major-mode-on-autoload-list'.
according to current `obarray'"
  (mapatoms
   (lambda (a)
     (if (symbolp a)
         (if (and (fmmm-major-mode-p a)
                  (not (memq a fmmm-initial-major-mode-list))
                  (not (memq a fmmm-major-mode-on-autoload-list)))
             (setq fmmm-major-mode-on-autoload-list
                   (cons a fmmm-major-mode-on-autoload-list)))))))

(defun fmmm-update-minor-mode-on-autoload-list ()
  "Update `fmmm-minor-mode-on-autoload-list'.
according to current `obarray'"
  (mapatoms
   (lambda (a)
     (if (symbolp a)
         (if (and (fmmm-minor-mode-p a)
                  (not (memq a fmmm-initial-minor-mode-list))
                  (not (memq a fmmm-minor-mode-on-autoload-list)))
             (setq fmmm-minor-mode-on-autoload-list
                   (cons a fmmm-minor-mode-on-autoload-list)))))))

;;;###autoload
(define-minor-mode fmmm-autoload-collector-mode
  "Minor mode for supporting fmmm autoload collecting system.
When enabled, load `fmmm-cache-file', if
`fmmm-major-mode-on-autoload-list', and
`fmmm-minor-mode-on-autoload-list' are nil.
In addition add hook
`fmmm-update-major-mode-on-autoload-list',
`fmmm-update-minor-mode-on-autoload-list',
and `fmmm-save-cache' to `kill-meacs-hook'"
  :group 'fmmm
  :global t
  (if fmmm-autoload-collector-mode
      (progn
        (if (and (null fmmm-major-mode-on-autoload-list)
                 (null fmmm-minor-mode-on-autoload-list))
            (load fmmm-cache-file t t t))
        (add-hook 'kill-emacs-hook
                  #'fmmm-update-major-mode-on-autoload-list)
        (add-hook 'kill-emacs-hook
                  #'fmmm-update-minor-mode-on-autoload-list)
        (add-hook 'kill-emacs-hook #'fmmm-save-cache))
    (fmmm-save-cache)
    (setq fmmm-major-mode-on-autoload-list nil
          fmmm-minor-mode-on-autoload-list nil)
    (remove-hook 'kill-emacs-hook
                 #'fmmm-update-major-mode-on-autoload-list)
    (remove-hook 'kill-emacs-hook
                 #'fmmm-update-minor-mode-on-autoload-list)
    (remove-hook 'kill-emacs-hook #'fmmm-save-cache)))

(provide 'fmmm)

;;; fmmm.el ends here
