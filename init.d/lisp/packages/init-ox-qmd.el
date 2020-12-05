
;;;; init-ox-qmd.el


(premise init)
(premise inst-ox-qmd)

(eval-when-compile (require 'ox-qmd))

(with-eval-after-load 'ox
  (require 'ox-qmd))

(with-eval-after-load 'ox-qmd
  (mapc (lambda (cell)
          (add-to-list 'ox-qmd-language-keyword-alist cell))
        '(("shell-script" . "bash")
                 ("yatex" . "latex"))))


(resolve init-ox-qmd)
