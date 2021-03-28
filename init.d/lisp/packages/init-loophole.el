
;;;; init-loophole.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise inst-loophole)

(eval-when-compile (require 'loophole))

(declare-function loophole-enable-map "loophole")
(declare-function loophole-disable-last-map "loophole")
(declare-function loophole-quit "loophole")
(declare-function loophole-start-edit "loophole")
(declare-function loophole-stop-edit "loophole")
(declare-function loophole-unset-key "loophole")
(declare-function loophole-disable-map "loophole")
(declare-function loophole-disable-all-maps "loophole")
(declare-function loophole-end-kmacro "loophole")
(declare-function loophole-abort-kmacro "loophole")
(declare-function loophole-mode-set-lighter-format "loophole")

(push '(loophole-mode . 20) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-]") #'loophole-set-key)
(overriding-set-key (kbd "C-c ] ]") #'loophole-mode)

(with-eval-after-load 'loophole
  (overriding-set-key (kbd "C-c ] e") #'loophole-enable-map)

  (setcdr loophole-mode-map nil)

  (defvar overriding-loophole-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-}") #'loophole-disable-last-map)
      (define-key map (kbd "C-{") #'loophole-quit)
      (define-key map (kbd "C-c ] q") #'loophole-quit)
      (define-key map (kbd "C-c ] e") #'loophole-start-edit)
      (define-key map (kbd "C-c ] c") #'loophole-stop-edit)
      (define-key map (kbd "C-c ] s") #'loophole-set-key)
      (define-key map (kbd "C-c ] u") #'loophole-unset-key)
      (define-key map (kbd "C-c ] d") #'loophole-disable-map)
      (define-key map (kbd "C-c ] D") #'loophole-disable-all-maps)
      (define-key map (kbd "C-c ] (") #'loophole-start-kmacro)
      (define-key map (kbd "C-c ] )") #'loophole-end-kmacro)
      (define-key map (kbd "C-c ] k s") #'loophole-start-kmacro)
      (define-key map (kbd "C-c ] k e") #'loophole-end-kmacro)
      (define-key map (kbd "C-c ] k a") #'loophole-abort-kmacro)
      (define-key map (kbd "C-c ] k b") #'loophole-bind-last-kmacro)
      (define-key map (kbd "C-c ] b e") #'loophole-bind-entry)
      (define-key map (kbd "C-c ] b c") #'loophole-bind-command)
      (define-key map (kbd "C-c ] b k") #'loophole-bind-kmacro)
      map)
    "Keymap for ‘loophole-mode’ which overrides global overriding maps.")

  (push `(loophole-mode . ,overriding-loophole-mode-map)
        overriding-reserved-key-map-alist)

  (loophole-mode-set-lighter-format 'tag))

(custom-set-variables '(loophole-kmacro-completing-key (kbd "C-]"))
                      '(loophole-bind-command-order
                        '(loophole-obtain-key-and-command-by-key-sequence
                          loophole-obtain-key-and-command-by-symbol))
                      '(loophole-bind-kmacro-order
                        '(loophole-obtain-key-and-kmacro-by-read-key
                          loophole-obtain-key-and-kmacro-by-recursive-edit
                          loophole-obtain-key-and-kmacro-by-recall-record))
                      '(loophole-set-key-order
                        '(loophole-obtain-key-and-command-by-key-sequence
                          loophole-obtain-key-and-kmacro-by-read-key
                          loophole-obtain-key-and-command-by-symbol
                          loophole-obtain-key-and-kmacro-by-recursive-edit
                          loophole-obtain-key-and-kmacro-by-recall-record)))


(resolve init-loophole)
