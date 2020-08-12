
;;;; init-autorevert.el


(premise init)

(eval-after-load 'autorevert
    (custom-set-variables '(auto-revert-mode-text " ARv")))


(resolve init-autorevert)
