
;;;; init-smartrep.el



;;; base

(require 'smartrep)

(custom-set-variables '(smartrep-mode-line-string-activated ""))


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

;; frame
(smartrep-define-key
 global-map "C-x 5"
 '(("0" . delete-frame)
   ("o" . other-frame)))


;;; org

(eval-after-load 'org
  '(progn
     (smartrep-define-key
         org-mode-map "C-c"
       '(("C-n" . (lambda () (outline-next-visible-heading 1)))
         ("C-p" . (lambda () (outline-previous-visible-heading 1)))
         ("C-f" . (lambda () (org-forward-heading-same-level 1)))
         ("C-b" . (lambda () (org-backward-heading-same-level 1)))
         ("C-^" . org-up-element)
         ("C-_" . org-down-element)))))


;;; git-gutter

(when (featurep 'git-gutter)
  (smartrep-define-key
      global-map "C-c v"
    '(("<" . git-gutter:previous-hunk)
      (">" . git-gutter:next-hunk)
      ("p" . git-gutter:previous-hunk)
      ("n" . git-gutter:next-hunk)
      ("d" . git-gutter:popup-hunk)
      ("r" . git-gutter:revert-hunk)
      ("s" . git-gutter:stage-hunk))))
