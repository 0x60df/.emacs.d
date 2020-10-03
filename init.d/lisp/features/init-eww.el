
;;;; init-eww.el


(premise init)

(eval-when-compile (require 'eww))

(setq eww-search-prefix "https://www.google.co.jp/search?q=")

(global-set-key "\C-cw" 'eww)


(resolve init-eww)
