
;;;; init-volatile-highlights.el


(premise init)
(premise inst-volatile-highlights)

(volatile-highlights-mode 1)
(eval-after-load 'volatile-highlights
  '(setcdr (assq 'volatile-highlights-mode minor-mode-alist) '("")))


(resolve init-volatile-highlights)
