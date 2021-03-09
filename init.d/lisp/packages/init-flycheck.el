
;;;; init-flycheck.el


(premise init)
(premise mode-line)
(premise bindings)
(premise inst-flycheck)

(push '(flycheck-mode . 45) mode-line-minor-mode-priority-alist)

(custom-set-variables
 '(flycheck-emacs-lisp-load-path 'inherit))

(overriding-set-key (kbd "C-c \"") #'flycheck-mode)
(overriding-set-key (kbd "C-l 2") #'flycheck-mode)

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
        overriding-reserved-key-map-alist))


(resolve init-flycheck)
