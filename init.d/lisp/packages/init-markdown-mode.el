
;;;; init-markdown-mode.el


(premise init)
(premise custom)
(premise inst-markdown-mode)

(eval-when-compile (require 'markdown-mode))

(custom-set-variables
 '(markdown-nested-imenu-heading-index nil))

(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("el" . emacs-lisp-mode)))

(defun markdown-show-trailing-whitespace ()
  "Show trailing whitespace."
  (interactive)
  (font-lock-add-keywords nil '(("[[:blank:]]+$" . 'trailing-whitespace)) t))

(defun markdown-hide-trailing-whitespace ()
  "Hide trailing whitespace."
  (interactive)
  (font-lock-remove-keywords nil '(("[[:blank:]]+$" . 'trailing-whitespace))))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook
            (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'markdown-mode-hook #'markdown-show-trailing-whitespace))


(resolve init-markdown-mode)
