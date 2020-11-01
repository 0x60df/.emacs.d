
;;;; init-multi-term.el


(premise init)
(premise bindings)
(premise inst-multi-term)

(overriding-set-key (kbd "H-t") #'multi-term)
(overriding-set-key (kbd "C-l t") #'multi-term)
(with-eval-after-load 'multi-term
  (custom-set-variables
               '(term-unbind-key-list
                 (append
                  (seq-filter (lambda (unbind-key)
                                (not (member unbind-key term-unbind-key-list)))
                              '("C-l"))
                  term-unbind-key-list)))
  (custom-set-variables
   '(term-bind-key-alist
     (append
      (seq-filter (lambda (bind-key)
                    (not (member bind-key term-bind-key-alist)))
                  '(("C-c C-j" . term-line-mode)
                    ("C-p" . term-send-raw)
                    ("C-n" . term-send-raw)
                    ("C-r" . term-send-raw)
                    ("C-s" . term-send-raw)))
      term-bind-key-alist))))

(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'multi-term
              (if (init-unit-p init-helm)
                  (custom-set-variables
                   '(term-unbind-key-list
                     (append
                      (seq-filter (lambda (unbind-key)
                                    (not (member unbind-key
                                                 term-unbind-key-list)))
                                  '("C-q"))
                      term-unbind-key-list)))))))


(resolve init-multi-term)
