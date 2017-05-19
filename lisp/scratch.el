;;; scratch.el --- generate new buffer instantly

;;; code:

(defgroup scratch nil
  "Generate new buffer instantly"
  :group 'emacs)

(defvar scratch-declared-major-mode nil
  "List of major mode which is declared for `scratch'")

(defun scratch-major-mode-p (symbol)
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

(defun scratch-collect-major-mode ()
  "Return list consist of major mode symbol."
  (let (l)
    (mapatoms
     (lambda (a) (if (scratch-major-mode-p a) (setq l (cons a l)))))
    l))

;;;###autoload
(defun scratch-declare-major-mode (&rest args)
  "Declare major mode for scratch."
  (mapc
   (lambda (arg)
     (if (symbolp arg) (add-to-list 'scratch-declared-major-mode arg)))
   args))

;;;###autoload
(defun scratch (arg)
  "Generate new buffer instantly."
  (interactive "p")
  (let ((s (intern
            (completing-read
             "Major mode: "
             (mapcar 'symbol-name (append
                                   scratch-declared-major-mode
                                   (scratch-collect-major-mode)))))))
    (let ((f (symbol-function s)))
      (if (autoloadp f) (autoload-do-load f)))
    (when (not (scratch-major-mode-p s))
      (setq scratch-declared-major-mode (delq s scratch-declared-major-mode))
      (delete s scratch-declared-major-mode)
      (setq s 'fundamental-mode))
    (switch-to-buffer (generate-new-buffer
                       (concat "*" (substring (format "%07x" (random)) -7)
                               "*")))
    (funcall s)))

(provide 'scratch)

;;; scratch.el ends here
