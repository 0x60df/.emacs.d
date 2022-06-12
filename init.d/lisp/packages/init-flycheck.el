
;;;; init-flycheck.el


(premise init)
(premise mode-line)
(premise bindings)
(premise inst-flycheck)

(push '(flycheck-mode . 44) mode-line-minor-mode-priority-alist)

(custom-set-variables
 '(flycheck-emacs-lisp-load-path 'inherit))

(overriding-set-key (kbd "C-c \"") #'flycheck-mode)
(overriding-set-key (kbd "C-l 2") #'flycheck-mode)

(add-to-list 'balance-mode-key-list (kbd "C-l 2"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC 2") . ,(kbd "l 2")))

(with-eval-after-load 'flycheck
  (defvar overriding-flycheck-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c \"") (make-sparse-keymap))
      (define-key map (kbd "C-c \" \"") #'flycheck-mode)

      (define-key map (kbd "C-l 2") (make-sparse-keymap))
      (define-key map (kbd "C-l 2 2") #'flycheck-mode)
      map)
    "Keymap for `flycheck-mode' which overrides global overriding maps.")

  (push `(flycheck-mode . ,overriding-flycheck-mode-map)
        overriding-reserved-key-map-alist)

  (defvar-local balance-mode-flycheck nil
    "`flycheck-mode' but t if `balance-mode' is active.")

  (defvar balance-mode-overriding-flycheck-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c \"") (make-sparse-keymap))
      (define-key map (kbd "c \" \"") #'flycheck-mode)

      (define-key map (kbd "l 2") (make-sparse-keymap))
      (define-key map (kbd "l 2 2") #'flycheck-mode)
      map)
    "Keymap for `flycheck-mode' which overrides global overriding maps.")

  (add-hook 'flycheck-mode-hook (lambda ()
                                  (if (and flycheck-mode balance-mode)
                                      (setq balance-mode-flycheck t)
                                    (setq balance-mode-flycheck nil))))

  (balance-mode-add-to-map-alist
   `(balance-mode-flycheck . ,balance-mode-overriding-flycheck-mode-map)))


(resolve init-flycheck)
