
;;;; init-scratch.el


(premise init)
(premise user-feature)
(premise bindings)

(eval-when-compile (require 'scratch))

(declare-function scratch-shred-all "scratch")
(declare-function scratch-shred "scratch")
(declare-function scratch-label "scratch")
(declare-function scratch-sticking-mode "scratch")
(declare-function scratch-auto-snapshot-mode "scratch")
(declare-function scratch-preserving-mode "scratch")

(overriding-set-key (kbd "C-c b") #'scratch)
(add-to-list 'balance-mode-key-list (kbd "C-c b"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC b") . ,(kbd "c b")))

(with-eval-after-load 'scratch
  (add-hook 'scratch-sticking-mode-hook
            (lambda ()
              (if (and scratch-sticking-mode
                       (memq (current-buffer) scratch-list))
                  (scratch-mode))))
  (add-hook 'scratch-sticking-mode-hook
            (lambda ()
              (if scratch-sticking-mode
                  (scratch-auto-snapshot-mode))))
  (add-hook 'scratch-sticking-mode-hook
            (lambda ()
              (if scratch-sticking-mode
                  (scratch-preserving-mode))))
  (add-hook 'scratch-hook #'scratch-sticking-mode)

  (add-hook 'scratch-before-label-hook (lambda () (scratch-mode 0)))

  (define-key scratch-mode-map [remap kill-buffer] #'scratch-shred)
  (define-key scratch-mode-map (kbd "C-c k") #'scratch-shred)
  (overriding-set-key (kbd "C-l C-b s") #'scratch-shred-all)
  (define-key scratch-mode-map (kbd "C-l C-b l") #'scratch-label)

  (add-to-list 'balance-mode-key-list (kbd "C-c k"))
  (add-to-list 'balance-mode-key-list (kbd "C-l b s"))
  (add-to-list 'balance-mode-key-list (kbd "C-l b l"))
  (add-to-list 'balance-mode-key-alias-alist
               `(,(kbd "c SPC k") . ,(kbd "c k")))
  (add-to-list 'balance-mode-key-alias-alist
               `(,(kbd "l SPC b") . ,(kbd "l b"))))


(resolve init-scratch)
