
;;;; init-eww.el


(premise init)
(premise custom)
(premise bindings)
(premise whitespace)

(custom-set-variables
 '(eww-search-prefix "https://www.google.co.jp/search?q="))

(global-set-key (kbd "C-c w") #'eww)

(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook (lambda () (setq show-trailing-whitespace nil))))


(resolve init-eww)
