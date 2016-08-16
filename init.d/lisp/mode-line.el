
;;;; mode-line.el



;;; format

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   "%Z"
   " "
   "%1*%1+"
   "%1@"
   (window-system (-2 (:eval (number-to-string (length (frame-list))))) "%F")
   " "
   (:propertize "%12b" face mode-line-buffer-identification-face)
   " "
   (:propertize ((-3 "%p")
                 (size-indication-mode (5 "/%I"))
                 (line-number-mode ((column-number-mode (7  " %l %c")
                                                        (4  " L%l")))
                                   ((column-number-mode (4  " C%c")))))
                face mode-line-position-face)
   (vc-mode (" " (:eval (propertize (replace-regexp-in-string
                                     "^\\s-+\\|\\s-+$" ""
                                     (substring-no-properties vc-mode))
                                    'face 'mode-line-vc-mode-face))))
   " "
   "%["
   (:propertize mode-name face mode-line-mode-name-face)
   mode-line-process
   (:propertize minor-mode-alist face mode-line-minor-mode-alist-face)
   "%n"
   "%]"
   (which-func-mode
    (" "
     (:propertize which-func-current face mode-line-which-func-mode-face)
     " "))
   (global-mode-string ("" global-mode-string " "))
   mode-line-end-spaces))


;;; faces

(defface mode-line-buffer-identification-face
  '((t (:weight bold)))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces)
(defface mode-line-position-face
  '((t ()))
  "Face used for position in the buffer parts of the mode line."
  :group 'mode-line-faces)
(defface mode-line-vc-mode-face
  '((t (:slant italic)))
  "Face used for vc-mode parts of the mode line."
  :group 'mode-line-faces)
(defface mode-line-mode-name-face
  '((t (:weight bold)))
  "Face used for mode-name parts of the mode line."
  :group 'mode-line-faces)
(defface mode-line-minor-mode-alist-face
  '((t ()))
  "Face used for minor-mode-alist parts of the mode line."
  :group 'mode-line-faces)
(defface mode-line-which-func-mode-face
  '((t (:slant italic)))
  "Face used for which-func-mode parts of the mode line."
  :group 'mode-line-faces)


;;; option setting

(which-function-mode 1)
(column-number-mode t)
(line-number-mode t)


;;; mode-name abbreviation

(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp I")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E-Lisp")))
(add-hook 'dired-mode-hook (lambda () (setq mode-name "Dired")))
(add-hook 'inferior-scheme-mode-hook (lambda () (setq mode-name "InfScm")))
(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " Abb")


;;; eol

(setq eol-mnemonic-dos "+")
(setq eol-mnemonic-mac "c")
(setq eol-mnemonic-unix "l")
