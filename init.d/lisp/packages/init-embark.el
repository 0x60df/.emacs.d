
;;;; init-embark.el


(premise init)
(premise bindings)
(premise inst-embark)

(overriding-set-key (kbd "C-^") #'embark-act)
(overriding-set-key (kbd "C-~") #'embark-dwim)
(overriding-set-key (kbd "C-q h b") #'embark-bindings)

(dolist (key (list (kbd "C-q h b"))) (add-to-list 'balance-mode-key-list key))

(add-to-list 'balance-mode-key-alias-alist `(,(kbd "q SPC h") . ,(kbd "q h")))

(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(resolve init-embark)
