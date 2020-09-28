
;;;; init-flex-isearch.el


(premise init)
(premise inst-flex-isearch)

(global-flex-isearch-mode 1)

(global-set-key (kbd "C-S-s") 'flex-isearch-forward)
(global-set-key (kbd "C-S-r") 'flex-isearch-backward)

(custom-set-variables
 '(flex-isearch-message-prefix
   (propertize "[FLEX] " 'face '(font-lock-constant-face))))


(resolve init-flex-isearch)
