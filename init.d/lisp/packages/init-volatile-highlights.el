
;;;; init-volatile-highlights.el


(premise init)
(premise inst-volatile-highlights)

(with-eval-after-load 'volatile-highlights
  (setcdr (assq 'volatile-highlights-mode minor-mode-alist) '("")))

(add-hook 'emacs-startup-hook #'volatile-highlights-mode)


(resolve init-volatile-highlights)
