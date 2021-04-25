
;;;; init-compile.el


(premise init)

(declare-function compilation-colorize-buffer load-file-name t t)

(with-eval-after-load 'compile
  (require 'ansi-color)

  (defun compilation-colorize-buffer ()
    "Colorize compilation buffer according to ANSI escape sequences."
    (read-only-mode 0)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))

  (add-hook 'compilation-filter-hook #'compilation-colorize-buffer))


(resolve init-compile)
