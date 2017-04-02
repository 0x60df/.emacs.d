
;;;; init-visual-regexp.el


(premise init)
(premise inst-visual-regexp)

(global-set-key (kbd "s-%") 'vr/query-replace)
(eval-after-load 'visual-regexp
  '(custom-set-variables '(vr/auto-show-help nil)
                         '(vr/default-replace-preview t)))


(resolve init-visual-regexp)
