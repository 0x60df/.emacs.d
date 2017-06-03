;;; fmmm.el --- functionalities for major/minor-mode

;;; code:

(defgroup fmmm nil
  "functionalities for major/minor-mode"
  :group 'emacs)

(defvar fmmm-declared-major-mode nil
  "List of major mode which is declared for `fmmm'")

(defvar fmmm-declared-minor-mode nil
  "List of minor mode which is declared for `fmmm'")

(defconst fmmm-minor-mode-variable-alist nil
  "Alist of minor-mode and minor-mode variable")

(defun fmmm-major-mode-p (symbol)
  "Non-nil if SYMBOL is not major mode."
  (if (and
       (commandp symbol)
       (string-match "-mode$" (symbol-name symbol))
       (not (memq symbol minor-mode-list)))
      (let ((byte-compile-warnings nil)
            (void-byte-code-function (make-byte-code nil nil nil nil)))
        (letrec ((trace-to-function
                  (lambda (s)
                    (let ((f (symbol-function s)))
                      (cond ((byte-code-function-p f) f)
                            ((autoloadp f) void-byte-code-function)
                            ((symbolp f)
                             (funcall trace-to-function f))
                            ((functionp f)
                             (byte-compile s)
                             (funcall trace-to-function s))
                            (t void-byte-code-function))))))
          (memq 'run-mode-hooks
                (append (aref (funcall trace-to-function symbol) 2) nil))))))

(defun fmmm-major-mode-list ()
  "Return list consist of major mode symbol."
  (mapc (lambda (s)
          (letrec ((trace-to-function
                    (lambda (ms)
                      (let ((f (symbol-function ms)))
                        (cond ((autoloadp f) f)
                              ((symbolp f)
                               (funcall trace-to-function f))
                              (t f))))))
            (if (and (not (autoloadp (funcall trace-to-function s)))
                     (not (fmmm-major-mode-p s)))
                (setq fmmm-declared-major-mode
                      (delq s fmmm-declared-major-mode)))))
        fmmm-declared-major-mode)
  (let (l)
    (mapatoms
     (lambda (a) (if (fmmm-major-mode-p a) (setq l (cons a l)))))
    (append fmmm-declared-major-mode l)))

;;;###autoload
(defun fmmm-declare-major-mode (&rest args)
  "Declare major mode for fmmm."
  (mapc
   (lambda (arg)
     (if (symbolp arg) (add-to-list 'fmmm-declared-major-mode arg)))
   args))

(defun fmmm-minor-mode-p (symbol)
  "Non-nil if SYMBOL is not minor mode."
  (and (memq symbol minor-mode-list)
       (string-match "-mode$" (symbol-name symbol))))

(defun fmmm-minor-mode-list ()
  "Return list consist of minor mode symbol."
  (mapc (lambda (s)
          (if (and (not (autoloadp (symbol-function s)))
                   (not (fmmm-minor-mode-p s)))
              (setq fmmm-declared-minor-mode
                    (delq s fmmm-declared-minor-mode))))
        fmmm-declared-minor-mode)
  (let (l)
    (mapatoms
     (lambda (a) (if (fmmm-minor-mode-p a) (setq l (cons a l)))))
    (append fmmm-declared-minor-mode l)))

(defun fmmm-enabled-minor-mode-list ()
  "Return list consist of enabled minor mode symbol."
  (let (l)
    (mapc (lambda (s)
            (if (if (assq s fmmm-minor-mode-variable-alist)
                    (let ((v (cdr (assq s fmmm-minor-mode-variable-alist))))
                      (and (boundp v) (symbol-value v)))
                  (and (boundp s) (symbol-value s)))
                (setq l (cons s l))))
          (fmmm-minor-mode-list))
    l))

(defun fmmm-disbled-minor-mode-list ()
  "Return list consist of disbled minor mode symbol."
  (let (l)
    (mapc (lambda (s)
            (if (not
                 (if (assq s fmmm-minor-mode-variable-alist)
                     (let ((v (cdr (assq s fmmm-minor-mode-variable-alist))))
                       (and (boundp v) (symbol-value v)))
                   (and (boundp s) (symbol-value s))))
                (setq l (cons s l))))
          (fmmm-minor-mode-list))
    l))

;;;###autoload
(defun fmmm-declare-minor-mode (&rest args)
  "Declare minor mode for fmmm."
  (mapc
   (lambda (arg)
     (if (symbolp arg) (add-to-list 'fmmm-declared-minor-mode arg)))
   args))

(provide 'fmmm)

;;; fmmm.el ends here
