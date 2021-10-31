
;;;; init-eglot.el


(premise init)
(premise feature)
(premise bindings)
(premise inst-eglot)

(eval-when-compile (require 'eglot))

(lazy-autoload 'eglot-current-server "eglot")
(lazy-autoload 'eglot-shutdown "eglot")

(declare-function eglot--modify-mode-line-format load-file-name t t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company)

  (let ((entry (assq 'eglot--managed-mode mode-line-misc-info)))
    (if (equal (caadr entry) " [")
        (setcar (cadr entry) ""))
    (if (equal (car (cddadr entry)) "] ")
        (setcar (cddadr entry) " ")))

  (defun eglot--modify-mode-line-format (return)
    "Advising `eglot--mode-line-format' to modify return."
    (if (listp return)
        (mapcar (lambda (element)
                  (if (not (and (consp element)
                                (eq (car element) :propertize)))
                      element
                    (if (equal "eglot" (cadr element))
                        (setcar (cdr element) "E"))
                    (mapc (lambda (prop)
                            (if (plist-get element prop)
                                (plist-put element prop nil)))
                          '(mouse-face keymap help-echo))
                    element))
                return)
      return))

  (advice-add 'eglot--mode-line-format
              :filter-return #'eglot--modify-mode-line-format)

  (require 'jsonrpc)
  (advice-add 'eglot :after (lambda (&rest _args)
                              (let ((process (jsonrpc--process
                                              (eglot-current-server))))
                                (if (process-live-p process)
                                    (set-process-query-on-exit-flag
                                     process nil))))))


(defun eglot-toggle ()
    "Toggle eglot"
    (interactive)
    (if (eglot-current-server)
        (call-interactively #'eglot-shutdown)
      (call-interactively #'eglot)))

(overriding-set-key (kbd "C-c c l") #'eglot-toggle)


(resolve init-eglot)
