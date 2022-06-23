
;;;; init-expand-region.el


(premise init)
(premise bindings)
(premise inst-expand-region)

(declare-function er/contract-region "expand-region")

(defun er/manipulate-region (arg)
  "Expand or contract region by semantic region.
ARG acts same as `er/expand-region'."
  (interactive "p")
  (funcall-interactively #'er/expand-region arg)
  (catch 'quit
    (while t
      (let ((key (read-key "Manipulating region...")))
        (cond ((eql key ?@) (funcall-interactively #'er/expand-region arg))
              ((eql key ?`) (funcall-interactively #'er/contract-region arg))
              (t (setq unread-command-events (list key)) (throw 'quit t)))))))

(overriding-set-key (kbd "C-`") #'er/expand-region)
(overriding-set-key (kbd "M-@") #'er/manipulate-region)
(with-eval-after-load 'expand-region
  (overriding-set-key (kbd "C-M-`") #'er/contract-region))

(add-to-list 'balance-mode-key-list (kbd "C-`"))


(resolve init-expand-region)
