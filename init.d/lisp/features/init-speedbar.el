
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

(defun speedbar-modify-mode-line-format (function &rest args)
  "Advising `speedbar-set-mode-line-format' to modify it."
  (setq mode-line-format mode-line-format-raw)
  (apply function args)
  (setq mode-line-format-raw mode-line-format)
  (setq mode-line-format
        (mode-line-format-auto-truncate
         (list mode-line-front-space mode-line-format-raw))))

(advice-add 'speedbar-set-mode-line-format
            :around #'speedbar-modify-mode-line-format)

(resolve init-speedbar)
