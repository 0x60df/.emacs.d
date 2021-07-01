
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
(declare-function loophole-name "loophole")
(declare-function loophole-end-kmacro "loophole")
(declare-function loophole-abort-kmacro "loophole")

(push '(loophole-mode . 13) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-]") #'loophole-dig)

(with-eval-after-load 'loophole
  (define-key loophole-mode-map (kbd "C-c [") nil)
  (define-key loophole-mode-map (kbd "C-c \\") nil)
  (define-key loophole-mode-map (kbd "C-c ] [") #'loophole-edit)
  (define-key loophole-mode-map (kbd "C-c ] ]") #'loophole-reveal)
  (define-key loophole-mode-map (kbd "C-c ] n") #'loophole-name)
  (define-key loophole-mode-map (kbd "C-c ] t") #'loophole-tag)
  (define-key loophole-mode-map (kbd "C-c ] : [") #'loophole-start-timer)
  (define-key loophole-mode-map (kbd "C-c ] : ]") #'loophole-stop-timer)
  (define-key loophole-mode-map (kbd "C-c ] : +") #'loophole-extend-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : [")
    #'loophole-start-editing-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : ]")
    #'loophole-stop-editing-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : +")
    #'loophole-extend-editing-timer)

  (defvar overriding-loophole-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-}") #'loophole-cover-latest)
      (define-key map (kbd "C-{") #'loophole-break)
      (define-key map (kbd "M-]") #'loophole-bury)
      map)
    "Keymap for ‘loophole-mode’ which overrides global overriding maps.")

  (define-key loophole-kmacro-by-recursive-edit-map (kbd "C-c [") nil)
  (define-key loophole-kmacro-by-recursive-edit-map (kbd "C-c \\") nil)
  (define-key loophole-kmacro-by-recursive-edit-map (kbd "C-]")
    #'loophole-end-kmacro)
  (define-key loophole-kmacro-by-recursive-edit-map (kbd "C-}")
    #'loophole-abort-kmacro)
  (define-key loophole-kmacro-by-recursive-edit-map (kbd "C-c ] k a")
    'undefined)

  (push `(loophole-mode . ,overriding-loophole-mode-map)
        overriding-reserved-key-map-alist)

  (add-hook 'loophole-write-lisp-mode-hook
            (lambda () (setq mode-name "Loophole Write Lisp"))))

(custom-set-variables
 '(loophole-use-auto-timer t)
 '(loophole-use-auto-editing-timer t)
 '(loophole-kmacro-by-read-key-finish-key (kbd "C-]"))
 '(loophole-array-by-read-key-finish-key (kbd "C-]"))
 '(loophole-set-key-order
   '(loophole-obtain-command-by-key-sequence
     (loophole-obtain-kmacro-by-read-key
      :key loophole-read-key-for-kmacro-by-read-key)
     loophole-obtain-command-by-read-command
     loophole-obtain-kmacro-by-recursive-edit
     loophole-obtain-command-by-lambda-form
     loophole-obtain-kmacro-by-recall-record
     loophole-obtain-keymap-by-read-keymap-variable
     loophole-obtain-keymap-by-read-keymap-function
     loophole-obtain-symbol-by-read-array-function
     loophole-obtain-object

     loophole-obtain-symbol-by-read-command
     loophole-obtain-symbol-by-read-keymap-function
     (loophole-obtain-array-by-read-key
      :key loophole-read-key-for-array-by-read-key)
     loophole-obtain-array-by-read-string))
 '(loophole-kmacro-by-recursive-edit-map-tag
   "<End: \\[loophole-end-kmacro], Abort: \\[loophole-abort-kmacro]>")
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
