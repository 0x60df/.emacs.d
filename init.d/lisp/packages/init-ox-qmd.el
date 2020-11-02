
;;;; init-ox-qmd.el


(premise init)

(with-eval-after-load 'ox
  (require 'ox-qmd))

(with-eval-after-load 'ox-qmd
  (setq ox-qmd-language-keyword-alist (append ox-qmd-language-keyword-alist
                                              '(("shell-script" . "bash")
                                                ("yatex" . "latex")))))


(resolve init-ox-qmd)
