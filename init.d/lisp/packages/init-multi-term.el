
;;;; init-multi-term.el


(premise init)
(premise inst-multi-term)

(global-set-key (kbd "H-t") 'multi-term)
(global-set-key (kbd "C-l t") 'multi-term)
(eval-after-load 'multi-term
  '(progn
     (custom-set-variables
      `(term-unbind-key-list
        '("C-q" ,@term-unbind-key-list))) ;work with helm
     (custom-set-variables
      `(term-bind-key-alist
        ',(append term-bind-key-alist
                  '(("C-c C-j" . term-line-mode)
                    ("C-p" . term-send-raw)
                    ("C-n" . term-send-raw)
                    ("C-r" . term-send-raw)
                    ("C-s" . term-send-raw)))))))


(resolve init-multi-term)
