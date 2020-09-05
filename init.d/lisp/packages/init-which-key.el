
;;;; init-which-key.el


(premise init)
(premise inst-which-key)

(eval-when-compile (require 'which-key))

(eval-after-load 'which-key
  '(progn
     (custom-set-variables '(which-key-idle-delay 0.4)
                           '(which-key-lighter "")
                           '(which-key-paging-key "C-?"))
     (mapc
      (lambda (s)
        (define-key which-key-mode-map
          (kbd (concat s " " which-key-paging-key))
          'which-key-C-h-dispatch))
      '("C-x" "C-c h" "C-q"))))

(which-key-mode 1)


(resolve init-which-key)
