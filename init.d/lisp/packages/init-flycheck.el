
;;;; init-flycheck.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-flycheck)

(overriding-set-key (kbd "C-c !") #'flycheck-mode)

(with-eval-after-load 'flycheck
  (defvar overriding-flycheck-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c !") (make-sparse-keymap))
      (define-key map (kbd "C-c ! !") #'flycheck-mode)
      map)
    "Keymap for `flycheck-mode' which overrides global overriding maps.")

  (push `(flycheck-mode . ,overriding-flycheck-mode-map)
        overriding-reserved-key-map-alist)

  (custom-set-variables
   '(flycheck-disabled-checkers
     (append
      flycheck-disabled-checkers
      (seq-filter (lambda (checker)
                    (not (member checker flycheck-disabled-checkers)))
                  '(emacs-lisp-checkdoc))))))


(resolve init-flycheck)
