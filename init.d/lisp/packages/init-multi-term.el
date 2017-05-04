
;;;; init-multi-term.el


(premise init)
(premise inst-multi-term)

(global-set-key (kbd "H-t") 'multi-term)
(eval-after-load 'multi-term
  '(progn
     (custom-set-variables
      `(term-unbind-key-list
        ',(cons "C-q" term-unbind-key-list))) ; work with helm
     (custom-set-variables
      `(term-bind-key-alist
        ',(append term-bind-key-alist
                  '(("C-p" . term-send-up)
                    ("C-n" . term-send-down)
                    ("M-p" . previous-line)
                    ("M-n" . next-line)
                    ("C-r" . term-send-reverse-search-history)
                    ("M-s" . isearch-forward)
                    ("M-r" . isearch-backward)))))))


(resolve init-multi-term)
