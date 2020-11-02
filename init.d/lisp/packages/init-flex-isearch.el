
;;;; init-flex-isearch.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-flex-isearch)

(defface flex-isearch-message-prefix
  '((t :weight bold))
  "Face for flex-isearch-message-prefix."
  :group 'user)

(custom-set-variables
 '(flex-isearch-message-prefix
   (propertize "[FLEX] " 'face 'flex-isearch-message-prefix)))

(overriding-set-key (kbd "C-S-s") #'flex-isearch-forward)
(overriding-set-key (kbd "C-S-r") #'flex-isearch-backward)

(add-hook 'emacs-startup-hook #'global-flex-isearch-mode)


(resolve init-flex-isearch)
