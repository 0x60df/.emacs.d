
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
  (overriding-set-key (kbd "C-l b s") #'scratch-shred-all)
  (define-key scratch-mode-map (kbd "C-l b l") #'scratch-label))


(resolve init-scratch)
