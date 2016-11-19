
;;;; init-ace-isearch.el


(global-ace-isearch-mode +1)
(eval-after-load 'ace-isearch
  '(custom-set-variables '(ace-isearch-jump-delay 0.6)
                         '(ace-isearch-lighter " AI")
                         '(ace-isearch-input-length 7)))
(define-key isearch-mode-map (kbd "C-a") 'ace-isearch-jump-during-isearch)
