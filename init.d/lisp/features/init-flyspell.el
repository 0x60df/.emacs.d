
;;;; init-flyspell.el


(eval-when-compile
  (require 'flyspell))

(global-set-key (kbd "C-c $") 'flyspell-mode)
(eval-after-load 'flyspell
  '(progn
     (define-key flyspell-mode-map (kbd "C-c $") nil)
     (define-key flyspell-mode-map (kbd "C-c $ $") 'flyspell-mode)
     (define-key flyspell-mode-map (kbd "C-,") nil)
     (define-key flyspell-mode-map (kbd "C-.") nil)
     (define-key flyspell-mode-map (kbd "C-;") nil)

     (define-key flyspell-mode-map (kbd "C-c $ >") 'flyspell-goto-next-error)
     (define-key flyspell-mode-map (kbd "C-c $ .") 'flyspell-auto-correct-word)
     (define-key flyspell-mode-map (kbd "C-c $ ,") 'flyspell-goto-next-error)
     (define-key flyspell-mode-map (kbd "C-c $ ;")
       'flyspell-auto-correct-previous-word)))


(resolve init-flyspell)
