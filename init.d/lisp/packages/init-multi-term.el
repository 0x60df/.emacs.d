
;;;; init-multi-term.el


(premise init)
(premise bindings)
(premise inst-multi-term)

(eval-when-compile (require 'multi-term))

(overriding-set-key (kbd "s-t") #'multi-term)

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
          ("C-s" . term-send-raw)))

  (advice-add 'multi-term :after (lambda (&rest _args)
                                   (let ((process (get-buffer-process
                                                   (current-buffer))))
                                     (if (process-live-p process)
                                         (set-process-query-on-exit-flag
                                          process nil))))))

(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'multi-term
              (if (init-unit-p init-helm)
                  (add-to-list 'term-unbind-key-list "C-q")))))


(resolve init-multi-term)
