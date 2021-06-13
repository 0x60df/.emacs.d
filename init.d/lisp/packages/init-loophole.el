
;;;; init-loophole.el


(premise init)
(premise custom)
(premise simple)
(premise mode-line)
(premise bindings)
(premise inst-loophole)

(eval-when-compile (require 'loophole))

(declare-function loophole-dig "loophole")
(declare-function loophole-edit "loophole")
(declare-function loophole-reveal "loophole")
(declare-function loophole-cover-latest "loophole")
(declare-function loophole-break "loophole")
(declare-function loophole-bury "loophole")
(declare-function loophole-mode-set-lighter-format "loophole")

(push '(loophole-mode . 13) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-]") #'loophole-dig)

(with-eval-after-load 'loophole
  (define-key loophole-mode-map (kbd "C-c [") nil)
  (define-key loophole-mode-map (kbd "C-c \\") nil)
  (define-key loophole-mode-map (kbd "C-c ] [") #'loophole-edit)
  (define-key loophole-mode-map (kbd "C-c ] ]") #'loophole-reveal)
  (define-key loophole-mode-map (kbd "C-c ] n") #'loophole-name)

  (defvar overriding-loophole-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-}") #'loophole-cover-latest)
      (define-key map (kbd "C-{") #'loophole-break)
      (define-key map (kbd "M-]") #'loophole-bury)
      map)
    "Keymap for ‘loophole-mode’ which overrides global overriding maps.")

  (push `(loophole-mode . ,overriding-loophole-mode-map)
        overriding-reserved-key-map-alist)

  (add-hook 'loophole-write-lisp-mode-hook
            (lambda () (setq mode-name "Loophole Write Lisp"))))

(custom-set-variables
 '(loophole-use-timer t)
 '(loophole-use-editing-timer t)
 '(loophole-kmacro-by-read-key-finish-key (kbd "C-]"))
 '(loophole-array-by-read-key-finish-key (kbd "C-]"))
 '(loophole-bind-command-order
   '(loophole-obtain-key-and-command-by-key-sequence
     loophole-obtain-key-and-command-by-read-command
     loophole-obtain-key-and-command-by-lambda-form
     loophole-obtain-key-and-object))
 '(loophole-bind-kmacro-order
   '(loophole-obtain-key-and-kmacro-by-read-key
     loophole-obtain-key-and-kmacro-by-recursive-edit
     loophole-obtain-key-and-kmacro-by-recall-record
     loophole-obtain-key-and-object))
 '(loophole-set-key-order
   '(loophole-obtain-key-and-command-by-key-sequence
     loophole-obtain-key-and-kmacro-by-read-key
     loophole-obtain-key-and-command-by-read-command
     loophole-obtain-key-and-kmacro-by-recursive-edit
     loophole-obtain-key-and-command-by-lambda-form
     loophole-obtain-key-and-kmacro-by-recall-record
     loophole-obtain-key-and-keymap-by-read-keymap-variable
     loophole-obtain-key-and-keymap-by-read-keymap-function
     loophole-obtain-key-and-object))
 '(loophole-mode-lighter-use-face t))

(loophole-define-map loophole-navigation-map
  '(("n" . next-line-scroll-up)
    ("p" . previous-line-scroll-down)
    ("f" . scroll-up-command)
    ("b" . scroll-down-command))
  "Keymap for simple navigation."
  loophole-navigation-map-state nil "State of `loophole-navigation-map'."
  "n")

(add-hook 'emacs-startup-hook #'loophole-mode)


(resolve init-loophole)
