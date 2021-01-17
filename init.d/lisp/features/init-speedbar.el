
;;;; init-speedbar.el


(premise init)
(premise custom)
(premise mode-line)

(custom-set-variables
 '(speedbar-use-images nil)
 '(speedbar-frame-parameters '((minibuffer . nil)
			       (width . 28)
			       (border-width . 0)
			       (menu-bar-lines . 0)
			       (tool-bar-lines . 0)
			       (unsplittable . t))))

(with-eval-after-load 'speedbar
  (advice-add 'speedbar-set-mode-line-format
              :around #'enhance-mode-line-format))

(resolve init-speedbar)
