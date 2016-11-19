
;;;; init-multi-term.el


(global-set-key (kbd "H-t") 'multi-term)
(eval-after-load 'multi-term
  '(custom-set-variables
   `(term-bind-key-alist ',(append term-bind-key-alist
                                   '(("C-p" . term-send-up)
                                     ("C-n" . term-send-down)
                                     ("M-p" . previous-line)
                                     ("M-n" . next-line)
                                     ("C-s" . nil)
                                     ("C-r" . term-send-reverse-search-history)
                                     ("M-s" . isearch-forward)
                                     ("M-r" . isearch-backward))))))
