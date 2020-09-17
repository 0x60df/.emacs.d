
;;;; init-smartrep.el



;;; base

(premise init)
(premise subr)
(premise inst-smartrep)

(require 'smartrep)

(eval-when-compile
  (require 'org)
  (require 'flyspell)
  (require 'smerge-mode))

(eval-after-load 'smartrep
  '(custom-set-variables '(smartrep-mode-line-string-activated "")))
(defcustom smartrep-mode-line-active-bg-adjuster nil ""
  :type 'function :group 'smartrep)
(call-with-runtime-bindings
 ((smartrep-mode-line-active-bg smartrep-mode-line-active-bg-adjuster))
 smartrep-map-internal bind-mode-line-active-bg)


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

;; frame alpha
(smartrep-define-key
    global-map "H-;"
  '(("s" . set-frame-alpha)
    ("p" . increase-frame-alpha)
    ("n" . decrease-frame-alpha)
    ("i" . increase-frame-alpha)
    ("d" . decrease-frame-alpha)))
(smartrep-define-key
    global-map "H-+"
  '(("s" . set-all-frames-alpha)
    ("p" . increase-all-frames-alpha)
    ("n" . decrease-all-frames-alpha)
    ("i" . increase-all-frames-alpha)
    ("d" . decrease-all-frames-alpha)))
(eval-after-load 'server
  '(smartrep-define-key
       global-map "C-H-+"
     '(("s" . server-set-all-client-frames-alpha)
       ("p" . server-increase-all-client-frames-alpha)
       ("n" . server-decrease-all-client-frames-alpha)
       ("i" . server-increase-all-client-frames-alpha)
       ("d" . server-decrease-all-client-frames-alpha))))

;; org
(eval-after-load 'org
  '(progn
     (define-key org-mode-map "\C-c\C-u" (lookup-key org-mode-map "\C-c\C-v"))
     (smartrep-define-key
         org-mode-map "C-c"
       '(("C-n" . (lambda () (outline-next-visible-heading 1)))
         ("C-p" . (lambda () (outline-previous-visible-heading 1)))
         ("C-f" . org-down-element)
         ("C-b" . org-up-element)
         ("C-v" . (lambda () (org-forward-heading-same-level 1)))
         ("M-v" . (lambda () (org-backward-heading-same-level 1)))))))

;; flyspell
(eval-after-load 'flyspell
  '(smartrep-define-key
       flyspell-mode-map "C-c $"
     '((">" . flyspell-goto-next-error)
       ("." . flyspell-auto-correct-word)
       ("," . flyspell-goto-next-error)
       (";" . flyspell-auto-correct-previous-word))))

;; smerge-mode

(eval-after-load 'smerge-mode
  '(smartrep-define-key
       smerge-mode-map "C-c ^"
     '(("RET" . smerge-keep-current)
       ("<return>" . smerge-keep-current)
       ("= <" . smerge-diff-base-mine)
       ("= =" . smerge-diff-mine-other)
       ("= >" . smerge-diff-base-other)
       ("C" . smerge-combine-with-next)
       ("E" . smerge-ediff)
       ("R" . smerge-refine)
       ("a" . smerge-keep-all)
       ("b" . smerge-keep-base)
       ("m" . smerge-keep-mine)
       ("n" . smerge-next)
       ("o" . smerge-keep-other)
       ("p" . smerge-prev)
       ("r" . smerge-resolve))))

;; page
(eval-after-load 'page
  '(smartrep-define-key
       global-map "C-x"
     '(("[" . backward-page)
       ("]" . forward-page))))

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
       ("s" . git-gutter:stage-hunk)
       ("q" . git-gutter:close-popup))))

;; multiple-cursors
(eval-after-load 'multiple-cursors
  '(smartrep-define-key
       global-map "C-c @"
     '(("p" . mc/mark-previous-like-this)
       ("n" . mc/mark-next-like-this)
       ("P". mc/unmark-next-like-this)
       ("N". mc/unmark-previous-like-this)
       ("C-v" . mc/cycle-forward)
       ("M-v" . mc/cycle-backward))))

;; dired-backs
(eval-after-load 'dired-subtree
  '(smartrep-define-key
       dired-mode-map ";"
     '(("b" . dired-subtree-up)
       ("f" . dired-subtree-down)
       ("n" . dired-subtree-next-sibling)
       ("p" . dired-subtree-previous-sibling)
       ("M-<" . dired-subtree-beginning)
       ("M->" . dired-subtree-end)
       ("m" . dired-subtree-mark-subtree)
       ("u" . dired-subtree-unmark-subtree)
       ("o f" . dired-subtree-only-this-file)
       ("o d" . dired-subtree-only-this-directory))))


(resolve init-smartrep)
