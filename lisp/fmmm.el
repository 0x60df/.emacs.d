;;; fmmm.el --- Feature for major/minor-mode

;;; Commentary:

;;; Code:

(require 'seq)

(defgroup fmmm nil
  "Feature for major/minor-mode"
  :group 'emacs)

(defcustom fmmm-cache-file (concat user-emacs-directory "fmmm-cache")
  "File which stores fmmm cache."
  :group 'fmmm
  :type 'file)

(defvar fmmm--major-mode-on-autoload-list nil
  "List of simbols of `major-mode' which will be autoloaded.")

(defvar fmmm--minor-mode-on-autoload-list nil
  "List of simbols of `minor-mode' which will be autoloaded.")

(defun fmmm-save-cache ()
  "Save fmmm-major/minor-mode-on-autoload-list to `fmmm-cache-file'."
  (with-temp-buffer
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m fmmm--major-mode-on-autoload-list)))
                  ',(reverse fmmm--major-mode-on-autoload-list))
           (current-buffer))
    (prin1 `(mapc (lambda (m)
                    (if (symbol-function m)
                        (push m fmmm--minor-mode-on-autoload-list)))
                  ',(reverse fmmm--minor-mode-on-autoload-list))
           (current-buffer))
    (write-file fmmm-cache-file)))

(defun fmmm-load-cache ()
  "Load fmmm-major/minor-mode-on-autoload-list from `fmmm-cache-file'."
  (load fmmm-cache-file t t t))

;;;###autoload
(defun fmmm-major-mode-p (symbol)
  "Non-nil if SYMBOL seems to be major mode.
This function obly can predicate loaded functions, in other
words, this function regard autoload as non major mode.
That's because autoload object does not contain the content
of function, hence it cannot be inferable that SYMBOL is
major mode."
  (or (memq symbol fmmm--major-mode-on-autoload-list)
      (letrec ((inspect
                (lambda (s)
                  (let ((f (symbol-function s)))
                    (cond ((null f) nil)
                          ((autoloadp f) nil)
                          ((byte-code-function-p f)
                           (let ((l (append (aref f 2) nil)))
                             (or (memq 'kill-all-local-variables l)
                                 (memq 'run-mode-hooks l))))
                          ((symbolp f) (funcall inspect f))
                          ((functionp f)
                           (or (get s 'derived-mode-parent)
                               (and (listp f)
                                    (let ((l (mapcar
                                              (lambda (e)
                                                (if (listp e) (car e)))
                                              f)))
                                      (or (memq 'kill-all-local-variables l)
                                          (memq 'run-mode-hooks l))))))
                          (t nil))))))
        (funcall inspect symbol))))

;;;###autoload
(defun fmmm-major-mode-list ()
  "Return list consist of symbol which seems to be major mode."
  (let (l)
    (mapatoms (lambda (a) (if (fmmm-major-mode-p a) (push a l))))
    l))

;;;###autoload
(defun fmmm-minor-mode-p (symbol)
  "Non-nil if SYMBOL seems to be minor mode.
This function obly can predicate loaded functions, in other
words, this function regard autoload as non major mode.
That's because minor mode symbol does not listed in any of
`minor-mode-list', `minor-mode-alist' and
`minor-mode-map-alist'."
  (or (memq symbol fmmm--minor-mode-on-autoload-list)
      (letrec ((inspect
                (lambda (s)
                  (let ((f (symbol-function s)))
                    (cond ((null f) s)
                          ((symbolp f) (funcall inspect f))
                          (t s))))))
        (let ((root (funcall inspect symbol)))
          (or (memq root minor-mode-list)
              (assq root minor-mode-alist)
              (assq root minor-mode-map-alist))))))

;;;###autoload
(defun fmmm-minor-mode-list ()
  "Return list consist of symbol which seems to be minor mode."
  (let (l)
    (mapatoms (lambda (a) (if (fmmm-minor-mode-p a) (push a l))))
    l))

(defun fmmm--minor-mode-variable-alist ()
  "Return alist of minor-mode and minor-mode variable.
Returned list only contains minor mode whose mode function
symbol is different from mode variable one."
  (let (l)
    (mapatoms (lambda (a)
                (if (symbolp a)
                    (let ((minor-mode-function (get a :minor-mode-function)))
                      (if minor-mode-function
                          (push (cons minor-mode-function a) l))))))
    l))

;;;###autoload
(defun fmmm-enabled-minor-mode-list ()
  "Return list of symbol which seems to be enabled minor mode."
  (let ((minor-mode-variable-alist (fmmm--minor-mode-variable-alist)))
    (seq-filter (lambda (s)
                  (let* ((cell (assq s minor-mode-variable-alist))
                         (state (if cell (cdr cell) s)))
                    (and (boundp state)
                         (symbol-value state))))
                (fmmm-minor-mode-list))))

;;;###autoload
(defun fmmm-disabled-minor-mode-list ()
  "Return list of symbol which seems to be disabled minor mode."
  (let ((minor-mode-variable-alist (fmmm--minor-mode-variable-alist)))
    (seq-filter (lambda (s)
                  (let* ((cell (assq s minor-mode-variable-alist))
                         (state (if cell (cdr cell) s)))
                    (not (and (boundp state)
                              (symbol-value state)))))
                (fmmm-minor-mode-list))))

(defconst fmmm--initial-major-mode-list (fmmm-major-mode-list)
  "List of simbols of `major-mode' which have been loaded on loading.")

(defconst fmmm--initial-minor-mode-list (fmmm-minor-mode-list)
  "List of simbols of `minor-mode' which have been loaded on loading.")

(defun fmmm-update-major-mode-on-autoload-list ()
  "Update `fmmm--major-mode-on-autoload-list'.
according to currently loaded functions which are reachable
via `obarray'."
  (mapatoms
   (lambda (a)
     (if (symbolp a)
         (if (and (fmmm-major-mode-p a)
                  (not (memq a fmmm--initial-major-mode-list))
                  (not (memq a fmmm--major-mode-on-autoload-list)))
             (push a fmmm--major-mode-on-autoload-list))))))

(defun fmmm-update-minor-mode-on-autoload-list ()
  "Update `fmmm--minor-mode-on-autoload-list'.
according to currently loaded functions which are reachable
via `obarray'."
  (mapatoms
   (lambda (a)
     (if (symbolp a)
         (if (and (fmmm-minor-mode-p a)
                  (not (memq a fmmm--initial-minor-mode-list))
                  (not (memq a fmmm--minor-mode-on-autoload-list)))
             (push a fmmm--minor-mode-on-autoload-list))))))

;;;###autoload
(define-minor-mode fmmm-autoload-collector-mode
  "Minor mode for supporting fmmm autoload collecting system.
When enabled, load `fmmm-cache-file', if
`fmmm--major-mode-on-autoload-list', and
`fmmm--minor-mode-on-autoload-list' are nil.
In addition add hook
`fmmm-update-major-mode-on-autoload-list',
`fmmm-update-minor-mode-on-autoload-list',
and `fmmm-save-cache' to `kill-meacs-hook'"
  :group 'fmmm
  :global t
  (if fmmm-autoload-collector-mode
      (progn
        (if (and (null fmmm--major-mode-on-autoload-list)
                 (null fmmm--minor-mode-on-autoload-list))
            (fmmm-load-cache))
        (add-hook 'kill-emacs-hook #'fmmm-save-cache)
        (add-hook 'kill-emacs-hook
                  #'fmmm-update-minor-mode-on-autoload-list)
        (add-hook 'kill-emacs-hook
                  #'fmmm-update-major-mode-on-autoload-list))
    (fmmm-update-major-mode-on-autoload-list)
    (fmmm-update-minor-mode-on-autoload-list)
    (fmmm-save-cache)
    (remove-hook 'kill-emacs-hook #'fmmm-save-cache)
    (remove-hook 'kill-emacs-hook
                 #'fmmm-update-minor-mode-on-autoload-list)
    (remove-hook 'kill-emacs-hook
                 #'fmmm-update-major-mode-on-autoload-list)))

(provide 'fmmm)

;;; fmmm.el ends here
