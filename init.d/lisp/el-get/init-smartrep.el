
;;;; init-smartrep.el



;;; base

(require 'smartrep)

(custom-set-variables '(smartrep-mode-line-string-activated ""))


;;; bindings


;; window
(smartrep-define-key
 global-map "C-x"
 '(("0" . delete-window)
   ("2" . split-window-below)
   ("3" . split-window-right)
   ("^" . enlarge-window)
   ("o" . other-window)
   ("{" . shrink-window-horizontally)
   ("}" . enlarge-window-horizontally)))

;; other window
(smartrep-define-key
    global-map "C-c ," '(("n" . (scroll-other-window 1))
                         ("p" . (scroll-other-window -1))
                         ("v" . 'scroll-other-window)
                         ("M-v" . (scroll-other-window '-))
                         ("M-<" . (beginning-of-buffer-other-window 0))
                         ("M->" . (end-of-buffer-other-window 0))))

;; frame
(smartrep-define-key
 global-map "C-x 5"
 '(("0" . delete-frame)
   ("o" . other-frame)))

;; org
(eval-after-load 'org
  '(smartrep-define-key
       org-mode-map "C-c"
     '(("C-n" . (lambda () (outline-next-visible-heading 1)))
       ("C-p" . (lambda () (outline-previous-visible-heading 1)))
       ("C-f" . (lambda () (org-forward-heading-same-level 1)))
       ("C-b" . (lambda () (org-backward-heading-same-level 1)))
       ("C-^" . org-up-element)
       ("C-_" . org-down-element))))

;; flyspell
(eval-after-load 'flyspell
  '(smartrep-define-key
       flyspell-mode-map "C-c $"
     '((">" . flyspell-goto-next-error)
       ("." . flyspell-auto-correct-word)
       ("," . flyspell-goto-next-error)
       (";" . flyspell-auto-correct-previous-word))))

;; git-gutter
(eval-after-load 'git-gutter
  '(smartrep-define-key
       global-map "C-c v"
     '(("<" . git-gutter:previous-hunk)
       (">" . git-gutter:next-hunk)
       ("p" . git-gutter:previous-hunk)
       ("n" . git-gutter:next-hunk)
       ("d" . git-gutter:popup-hunk)
       ("r" . git-gutter:revert-hunk)
       ("s" . git-gutter:stage-hunk))))

;; multiple-cursors
(eval-after-load 'multiple-cursors
  '(smartrep-define-key
       global-map "C-c @"
     '(("p" . mc/mark-previous-like-this)
       ("n" . mc/mark-next-like-this))))
