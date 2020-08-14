;;; fmmm.el --- functionalities for major/minor-mode

;;; code:

(defgroup fmmm nil
  "functionalities for major/minor-mode"
  :group 'emacs)

;;;###autoload
(defcustom fmmm-complementary-major-mode-list nil
  "List of simbols which are considered as major-mode in `fmmm'" :group 'fmmm)

;;;###autoload
(defcustom fmmm-complementary-minor-mode-list nil
  "List of simbols which are considered as minor-mode in `fmmm'" :group 'fmmm)

(defvar fmmm-minor-mode-variable-alist
  (let (l)
    (mapatoms (lambda (a)
                (if (symbolp a)
                    (let ((minor-mode-function (get a :minor-mode-function)))
                      (if minor-mode-function
                          (setq l (cons (cons minor-mode-function a) l)))))))
    l)
  "Alist of minor-mode and minor-mode variable")

(defun fmmm-major-mode-p (symbol)
  "Non-nil if SYMBOL is not major mode."
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
                            (not (memq a valid-complemntary-list)))
                       (setq l (cons a l)))))
      (append valid-complemntary-list l))))

(defun fmmm-minor-mode-p (symbol)
  "Non-nil if SYMBOL is not minor mode."
  (memq symbol minor-mode-list))

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
      (append valid-complemntary-list l))))

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

(defun fmmm-update-minor-mode-variable-alist (&optional args)
  "Update `fmmm-minor-mode-variable-alist'.
according to current `obarray'"
  (let (l)
    (mapatoms (lambda (a)
                (if (symbolp a)
                    (let ((minor-mode-function (get a :minor-mode-function)))
                      (if minor-mode-function
                          (setq l (cons (cons minor-mode-function a) l)))))))
    (setq fmmm-minor-mode-variable-alist l)))

(provide 'fmmm)

;;; fmmm.el ends here
