
;;;; init-multi-term.el


(premise init)
(premise bindings)
(premise inst-multi-term)

(overriding-set-key (kbd "H-t") #'multi-term)
(overriding-set-key (kbd "C-l t") #'multi-term)
(with-eval-after-load 'multi-term
  (add-to-list 'term-unbind-key-list "C-l")

  (mapc (lambda (cell)
          (let ((assoc (assoc (car cell) term-bind-key-alist)))
            (if assoc
                (setcdr assoc (cdr cell))
              (setq term-bind-key-alist (cons cell term-bind-key-alist)))))
        '(("C-c C-j" . term-line-mode)
          ("C-p" . term-send-raw)
          ("C-n" . term-send-raw)
          ("C-r" . term-send-raw)
          ("C-s" . term-send-raw))))

(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'multi-term
              (if (init-unit-p init-helm)
                  (add-to-list 'term-unbind-key-list "C-q")))))


(resolve init-multi-term)
