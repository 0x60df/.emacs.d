
;;;; init-scratch.el


(premise init)
(premise user-feature)
(premise bindings)

(eval-when-compile (require 'scratch))

(declare-function scratch-shred-all "scratch")
(declare-function scratch-shred "scratch")
(declare-function scratch-label "scratch")

(overriding-set-key (kbd "C-c b") #'scratch)

(with-eval-after-load 'scratch
  (overriding-set-key (kbd "C-l b s") #'scratch-shred-all)
  (define-key scratch-mode-map (kbd "C-c k") #'scratch-shred)
  (define-key scratch-mode-map (kbd "C-l b l") #'scratch-label))


(resolve scratch)
