
;;;; init-ediff.el


(eval-after-load 'ediff-wind
  '(custom-set-variables
    '(ediff-window-setup-function 'ediff-setup-windows-plain)))
