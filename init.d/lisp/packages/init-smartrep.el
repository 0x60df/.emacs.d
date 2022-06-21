
;;;; init-smartrep.el


(premise init)
(premise custom)
(premise simple)
(premise bindings)
(premise mode-line)
(premise init-flyspell)
(premise init-flymake)
(premise inst-smartrep)

(eval-when-compile
  (require 'org)
  (require 'dired)
  (require 'smerge-mode)
  (require 'flyspell)
  (require 'flymake)
  (require 'evil-states))

(declare-function smartrep-define-key "smartrep")

;;; settings

(custom-set-variables
 '(smartrep-mode-line-string-activated " SRe"))

(with-eval-after-load 'smartrep
  (letrec ((insert-before-recursive-edit-close
            (lambda (l)
              (cond ((null l) l)
                    ((equal (cadr l) "%]")
                     (setcdr l (cons '(:propertize
                                       smartrep-mode-line-string
                                       face mode-line-warning)
                                     (cdr l))))
                    (t (funcall insert-before-recursive-edit-close (cdr l)))))))
    (funcall insert-before-recursive-edit-close mode-line-modes)))

(add-hook 'emacs-startup-hook (lambda () (require 'smartrep)))



;;; bindings

(with-eval-after-load 'smartrep

  ;; undo
  (let ((key "C-x"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("u" . undo))))

  ;; navigation
  (let ((key "C-l"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("f" . emulate-forward-word)
        ("b" . emulate-backward-word))))
  (let ((key "C-l"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("v" . emulate-scroll-down-command))))

  ;; page
  (let ((key "C-x"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("[" . backward-page)
        ("]" . forward-page))))

  ;; window
  (let ((key "C-x"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("0" . delete-window)
        ("1" . delete-other-windows)
        ("2" . split-window-below)
        ("3" . split-window-right)
        ("^" . enlarge-window)
        ("o" . other-window)
        ("{" . shrink-window-horizontally)
        ("}" . enlarge-window-horizontally))))

  ;; frame
  (let ((key "C-x 5"))
    (smartrep-define-key (overriding-map-for (kbd key)) key
      '(("0" . delete-frame)
        ("o" . other-frame))))

  ;; org
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-u")
      (lookup-key org-mode-map (kbd "C-c C-v")))
    (smartrep-define-key org-mode-map "C-c"
      '(("C-n" . (outline-next-visible-heading 1))
        ("C-p" . (outline-previous-visible-heading 1))
        ("C-f" . org-down-element)
        ("C-b" . org-up-element)
        ("C-v" . (org-forward-heading-same-level 1))
        ("M-v" . (org-backward-heading-same-level 1)))))

  ;; flymake
  (with-eval-after-load 'flymake
    (smartrep-define-key overriding-flymake-mode-map "C-l 3"
      '(("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error)))
    (smartrep-define-key balance-mode-overriding-flymake-mode-map "l 3"
      '(("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error))))

  ;; flyspell
  (with-eval-after-load 'flyspell
    (smartrep-define-key overriding-flyspell-mode-map "C-l 4"
       '((">" . flyspell-goto-next-error)
         ("." . flyspell-auto-correct-word)
         ("," . flyspell-goto-next-error)
         (";" . flyspell-auto-correct-previous-word)))
    (smartrep-define-key balance-mode-overriding-flyspell-mode-map "l 4"
       '((">" . flyspell-goto-next-error)
         ("." . flyspell-auto-correct-word)
         ("," . flyspell-goto-next-error)
         (";" . flyspell-auto-correct-previous-word))))

  ;; smerge-mode
  (with-eval-after-load 'smerge-mode
    (smartrep-define-key smerge-mode-map "C-c ^"
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
        ("l" . smerge-keep-lower)
        ("m" . smerge-keep-mine)
        ("n" . smerge-next)
        ("o" . smerge-keep-other)
        ("p" . smerge-prev)
        ("r" . smerge-resolve)
        ("u" . smerge-keep-upper)))))

(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'smartrep

              ;; multiple-cursors
              (when (init-unit-p inst-multiple-cursors)
                (let ((key "C-c @"))
                  (smartrep-define-key (overriding-map-for (kbd key)) key
                    '(("p" . mc/mark-previous-like-this)
                      ("n" . mc/mark-next-like-this)
                      ("P". mc/unmark-next-like-this)
                      ("N". mc/unmark-previous-like-this)
                      ("C-v" . mc/cycle-forward)
                      ("M-v" . mc/cycle-backward))))
                (if (init-unit-p inst-evil)
                    (with-eval-after-load 'evil
                      (let ((key "SPC @"))
                        (smartrep-define-key evil-normal-state-map key
                          '(("p" . mc/mark-previous-like-this)
                            ("n" . mc/mark-next-like-this)
                            ("P". mc/unmark-next-like-this)
                            ("N". mc/unmark-previous-like-this)
                            ("C-v" . mc/cycle-forward)
                            ("M-v" . mc/cycle-backward)))))))

              ;; git-gutter
              (if (init-unit-p inst-git-gutter-fringe)
                  (with-eval-after-load 'git-gutter
                    (let ((key "C-c v"))
                      (smartrep-define-key (overriding-map-for (kbd key)) key
                        '(("<" . git-gutter:previous-hunk)
                          (">" . git-gutter:next-hunk)
                          ("p" . git-gutter:previous-hunk)
                          ("n" . git-gutter:next-hunk)
                          ("d" . git-gutter:popup-hunk)
                          ("r" . git-gutter:revert-hunk)
                          ("s" . git-gutter:stage-hunk)
                          ("q" . git-gutter:close-popup))))
                    (let ((key "c v"))
                      (smartrep-define-key overriding-balance-mode-map key
                        '(("<" . git-gutter:previous-hunk)
                          (">" . git-gutter:next-hunk)
                          ("p" . git-gutter:previous-hunk)
                          ("n" . git-gutter:next-hunk)
                          ("d" . git-gutter:popup-hunk)
                          ("r" . git-gutter:revert-hunk)
                          ("s" . git-gutter:stage-hunk)
                          ("q" . git-gutter:close-popup))))))

              ;; dired-backs
              (if (init-unit-p inst-dired-hacks)
                  (with-eval-after-load 'dired
                    (smartrep-define-key
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
                       ("o d" . dired-subtree-only-this-directory))))))))


(resolve init-smartrep)
