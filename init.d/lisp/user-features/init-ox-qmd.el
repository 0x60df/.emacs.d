
;;;; init-ox-qmd.el


(eval-when-compile
  (require 'org-install)
  (require 'ox-qmd))

(eval-after-load 'org
  '(progn
     (require 'ox-qmd)

     (setq ox-qmd-language-keyword-alist
           (append ox-qmd-language-keyword-alist
                   '(("shell-script" . "bash")
                     ("yatex" . "latex"))))))


(resolve init-ox-qmd)
