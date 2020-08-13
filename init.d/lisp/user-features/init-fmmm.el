
;;;; init-fmmm.el


(premise init)

(eval-after-load 'fmmm
  (custom-set-variables '(fmmm-complementary-major-mode
                          '(shell-script-mode ruby-mode))))

(resolve fmmm)
