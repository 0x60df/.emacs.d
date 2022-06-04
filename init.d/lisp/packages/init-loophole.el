
;;;; init-loophole.el


(premise init)
(premise custom)
(premise simple)
(premise mode-line)
(premise bindings)
(premise inst-loophole)

(eval-when-compile (require 'loophole))

(declare-function loophole-open "loophole")
(declare-function loophole-edit "loophole")
(declare-function loophole-reveal "loophole")
(declare-function loophole-cover-latest "loophole")
(declare-function loophole-break "loophole")
(declare-function loophole-close "loophole")
(declare-function loophole-name "loophole")
(declare-function loophole-end-kmacro "loophole")
(declare-function loophole-abort-kmacro "loophole")
(declare-function loophole-disable "loophole")
(declare-function loophole-enable "loophole")
(declare-function loophole-unregister "loophole")
(declare-function loophole-registered-p "loophole")
(declare-function loophole-map-variable-for-key-binding "loophole")
(declare-function loophole-start-editing "loophole")
(declare-function loophole-save "loohole")
(declare-function loophole-load "loohole")
(declare-function loophole-tag "loohole")
(declare-function loophole-globalize "loohole")
(declare-function loophole-localize "loohole")
(declare-function loophole-start-timer "loohole")
(declare-function loophole-stop-timer "loohole")
(declare-function loophole-extend-timer "loohole")
(declare-function loophole-start-editing-timer "loohole")
(declare-function loophole-stop-editing-timer "loohole")
(declare-function loophole-extend-editing-timer "loohole")

(push '(loophole-mode . 13) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-]") #'loophole-open)
(add-to-list 'balance-mode-key-list (kbd "C-]"))

(with-eval-after-load 'loophole
  (define-key loophole-mode-map (kbd "C-c [") nil)
  (define-key loophole-mode-map (kbd "C-c \\") nil)
  (define-key loophole-mode-map (kbd "C-c ] [") #'loophole-edit)
  (define-key loophole-mode-map (kbd "C-c ] ]") #'loophole-reveal)
  (define-key loophole-mode-map (kbd "C-c ] n") #'loophole-name)
  (define-key loophole-mode-map (kbd "C-c ] t") #'loophole-tag)
  (define-key loophole-mode-map (kbd "C-c ] g") #'loophole-globalize)
  (define-key loophole-mode-map (kbd "C-c ] l") #'loophole-localize)
  (define-key loophole-mode-map (kbd "C-c ] : [") #'loophole-start-timer)
  (define-key loophole-mode-map (kbd "C-c ] : ]") #'loophole-stop-timer)
  (define-key loophole-mode-map (kbd "C-c ] : +") #'loophole-extend-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : [")
    #'loophole-start-editing-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : ]")
    #'loophole-stop-editing-timer)
  (define-key loophole-mode-map (kbd "C-c ] : : +")
    #'loophole-extend-editing-timer)

  (loophole-load)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (loophole-save
               (lambda (map-variable)
                 (let ((name (symbol-name map-variable)))
                   (and (string-match "^loophole-.+-map$" name)
                        (not (string-equal name
                                           "loophole-navigation-map"))))))))

  (defvar overriding-loophole-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-}") #'loophole-cover-latest)
      (define-key map (kbd "ESC M-]") #'loophole-cover-latest)
      (define-key map (kbd "C-{") #'loophole-break)
      (define-key map (kbd "ESC M-[") #'loophole-break)
      (define-key map (kbd "M-]") #'loophole-close)
      map)
    "Keymap for ‘loophole-mode’ which overrides global overriding maps.")

  (define-key loophole-defining-kmacro-map (kbd "C-c [") nil)
  (define-key loophole-defining-kmacro-map (kbd "C-c \\") nil)
  (define-key loophole-defining-kmacro-map (kbd "C-]")
    #'loophole-end-kmacro)
  (define-key loophole-defining-kmacro-map (kbd "C-}")
    #'loophole-abort-kmacro)
  (define-key loophole-defining-kmacro-map (kbd "C-c ] k a")
    'undefined)

  (add-to-list 'balance-mode-key-list (kbd "C-c ] ["))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ]"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] n"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] t"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] g"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] l"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : ["))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : ]"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : +"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : : ["))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : : ]"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] : : +"))
  (add-to-list 'balance-mode-key-list (kbd "C-}"))
  (add-to-list 'balance-mode-key-list (kbd "C-{"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] {"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] }"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ^"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] >"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] <"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ["))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ]"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] -"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ="))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ;"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] #"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] /"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ,"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ."))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] q"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] p"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] n"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] h"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] s"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] u"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] r"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] e"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] d"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] D"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] c d"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] c m"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] \\"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] ("))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] )"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] k s"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] k e"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] k a"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] k b"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b e"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b c"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b k"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b K"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b a"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b m"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] b s"))
  (add-to-list 'balance-mode-key-list (kbd "C-c ] m"))

  (add-to-list 'balance-mode-key-alias-alist `(,(kbd "c SPC ]") . ,(kbd "c ]")))

  (push `(loophole-mode . ,overriding-loophole-mode-map)
        overriding-reserved-key-map-alist)

  (add-hook 'loophole-after-start-editing-functions #'loophole-enable)
  (add-hook 'loophole-after-globalize-functions #'loophole-enable)
  (add-hook 'loophole-after-merge-functions #'loophole-start-editing)

  (defvar loophole-navigation-map
    (let ((map (make-sparse-keymap)))
      (mapc (lambda (k)
              (define-key map (vector k)
                (lambda (n)
                  (interactive "p")
                  (loophole-disable (loophole-map-variable-for-key-binding
                                     (this-command-keys)))
                  (if (eq (get major-mode 'mode-class) 'special)
                      (message "Loophole navigation map is disabled")
                    (self-insert-command n)))))
            (vconcat " !\"#$%&'()*+,-./0123456789:;<=>?"
                     "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                     "`acdeghijklmoqrstuvwxyz{|}~"))
      (define-key map (kbd "n") #'next-line-scroll-up)
      (define-key map (kbd "p") #'previous-line-scroll-down)
      (define-key map (kbd "f") #'scroll-up-command)
      (define-key map (kbd "b") #'scroll-down-command)
      map)
    "Keymap for simple navigation.")

  (defvar loophole-navigation-map-state nil
    "State of `loophole-navigation-map'.")

  (if (loophole-registered-p 'loophole-navigation-map)
      (loophole-unregister 'loophole-navigation-map))
  (loophole-register
   'loophole-navigation-map 'loophole-navigation-map-state "n"))

(custom-set-variables
 '(loophole-decide-obtaining-method-after-read-key nil)
 '(loophole-use-auto-timer t)
 '(loophole-use-auto-editing-timer t)
 '(loophole-use-auto-start-editing-for-existing-binding t)
 '(loophole-use-idle-prioritize t)
 '(loophole-read-key-termination-key (kbd "C-]"))
 '(loophole-read-buffer-inhibit-recursive-edit t)
 '(loophole-set-key-order
   '(loophole-obtain-command-by-key-sequence
     loophole-obtain-kmacro-on-top-level
     (loophole-obtain-command-by-read-command
      :key loophole-read-key-with-time-limit)
     loophole-obtain-kmacro-by-read-key
     loophole-obtain-command-by-lambda-form
     loophole-obtain-kmacro-by-recall-record
     loophole-obtain-symbol-by-read-keymap-function
     loophole-obtain-keymap-by-read-keymap-variable
     loophole-obtain-array-by-read-key
     (loophole-obtain-object :key loophole-read-key-with-time-limit)

     loophole-obtain-array-by-read-string
     loophole-obtain-symbol-by-read-array-function

     loophole-obtain-kmacro-by-recursive-edit
     loophole-obtain-symbol-by-read-command
     loophole-obtain-keymap-by-read-keymap-function))
 '(loophole-defining-kmacro-map-tag
   "<End: \\[loophole-end-kmacro], Abort: \\[loophole-abort-kmacro]>")
 '(loophole-make-load-overwrite-map t)
 '(loophole-use-idle-save
   '((lambda (map-variable)
       (let ((name (symbol-name map-variable)))
         (and (string-match "^loophole-.+-map$" name)
              (not (string-equal name "loophole-navigation-map")))))))
 '(loophole-mode-lighter-use-face t))

(add-hook 'emacs-startup-hook #'loophole-mode)


(resolve init-loophole)
