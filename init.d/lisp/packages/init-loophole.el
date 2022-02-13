
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
 '(loophole-use-auto-timer t)
 '(loophole-use-auto-editing-timer t)
 '(loophole-use-idle-prioritize t)
 '(loophole-kmacro-by-read-key-finish-key (kbd "C-]"))
 '(loophole-array-by-read-key-finish-key (kbd "C-]"))
 '(loophole-read-buffer-inhibit-recursive-edit t)
 '(loophole-set-key-order
   '(loophole-obtain-command-by-key-sequence
     loophole-obtain-kmacro-on-top-level
     (loophole-obtain-command-by-read-command
      :key loophole-read-key-with-time-limit)
     (loophole-obtain-kmacro-by-read-key
      :key loophole-read-key-for-kmacro-by-read-key)
     loophole-obtain-command-by-lambda-form
     loophole-obtain-kmacro-by-recall-record
     loophole-obtain-symbol-by-read-keymap-function
     loophole-obtain-keymap-by-read-keymap-variable
     (loophole-obtain-array-by-read-key
      :key loophole-read-key-for-array-by-read-key)
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
