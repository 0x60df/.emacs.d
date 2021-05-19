
;;;; init-loophole.el


(premise init)
(premise custom)
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
(declare-function loophole-turn-on-auto-prioritize "loophole")
(declare-function loophole-turn-on-auto-stop-editing "loophole")
(declare-function loophole-turn-on-auto-resume "loophole")
(declare-function loophole-mode-set-lighter-format "loophole")

(push '(loophole-mode . 13) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-]") #'loophole-dig)

(with-eval-after-load 'loophole
  (define-key loophole-mode-map (kbd "C-c [") nil)
  (define-key loophole-mode-map (kbd "C-c \\") nil)
  (define-key loophole-mode-map (kbd "C-c ] [") #'loophole-edit)
  (define-key loophole-mode-map (kbd "C-c ] ]") #'loophole-reveal)

  (defvar overriding-loophole-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-}") #'loophole-cover-latest)
      (define-key map (kbd "C-{") #'loophole-break)
      (define-key map (kbd "M-]") #'loophole-bury)
      map)
    "Keymap for ‘loophole-mode’ which overrides global overriding maps.")

  (push `(loophole-mode . ,overriding-loophole-mode-map)
        overriding-reserved-key-map-alist)

  (loophole-mode-set-lighter-format 'tag)

  (loophole-turn-on-auto-prioritize)
  (loophole-turn-on-auto-stop-editing)
  (loophole-turn-on-auto-resume)

  (add-hook 'loophole-write-lisp-mode-hook
            (lambda () (setq mode-name "Loophole Write Lisp"))))

(custom-set-variables '(loophole-kmacro-finish-key (kbd "C-]"))
                      '(loophole-use-timer t)
                      '(loophole-bind-command-order
                        '(loophole-obtain-key-and-command-by-key-sequence
                          loophole-obtain-key-and-command-by-symbol
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
                          loophole-obtain-key-and-command-by-symbol
                          loophole-obtain-key-and-kmacro-by-recursive-edit
                          loophole-obtain-key-and-command-by-lambda-form
                          loophole-obtain-key-and-kmacro-by-recall-record
                          loophole-obtain-key-and-object))
                      '(loophole-mode-lighter-use-face t))

(add-hook 'emacs-startup-hook #'loophole-mode)


(resolve init-loophole)
