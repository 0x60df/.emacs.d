
;;;; init-scratch.el


(premise init)
(premise user-feature)
(premise bindings)

(eval-when-compile (require 'scratch))

(declare-function scratch-shred-all "scratch")
(declare-function scratch-shred "scratch")
(declare-function scratch-label "scratch")
(declare-function scratch-sticky-mode "scratch")
(declare-function scratch-auto-snapshot-mode "scratch")

(overriding-set-key (kbd "C-c b") #'scratch)

(with-eval-after-load 'scratch
  (add-hook 'scratch-hook #'scratch-sticky-mode)
  (add-hook 'scratch-hook #'scratch-auto-snapshot-mode)

  (define-key scratch-mode-map [remap kill-buffer] #'scratch-shred)
  (overriding-set-key (kbd "C-l b s") #'scratch-shred-all)
  (define-key scratch-mode-map (kbd "C-l b l") #'scratch-label))


(resolve init-scratch)
