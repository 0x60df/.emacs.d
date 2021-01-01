
;;;; init-elisp-mode.el


(premise init)

(eval-when-compile (require 'find-func))

(declare-function elisp--xref-find-definitions-with-unit t t)

(with-eval-after-load 'elisp-mode
  (add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp I")))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E-Lisp")))

  (with-eval-after-load 'find-func
    (defconst find-unit-regexp ";;;; "
      "The regexp used by `xref-find-definitions' for searching unit.")
    (push '(unit . find-unit-regexp) find-function-regexp-alist))

  (defun elisp--xref-find-definitions-with-unit (function symbol)
    "Advising `elisp--xref-find-definitions' to add unit definition."
    (let ((definitions (funcall function symbol))
          (definitions-for-unit nil))
      (mapc (lambda (prefix)
              (let* ((name (concat prefix (symbol-name symbol)))
                     (unit (intern name))
                     (elc (cdr (assq unit init-units))))
                (if elc
                    (let ((el (locate-file
                               name `(,(file-name-directory elc)) '(".el"))))
                      (push (elisp--xref-make-xref 'unit unit (if el el elc))
                            definitions-for-unit)))))
            '("" "init-" "inst-"))
      (append definitions definitions-for-unit)))
  (advice-add 'elisp--xref-find-definitions
              :around #'elisp--xref-find-definitions-with-unit))



(resolve init-elisp-mode)
