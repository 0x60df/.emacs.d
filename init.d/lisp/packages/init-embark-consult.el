
;;;; init-embark-consult.el


(premise init)
(premise inst-consult)
(premise inst-embark-consult)

(with-eval-after-load 'embark
  (require 'consult)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(resolve init-embark-consult)
