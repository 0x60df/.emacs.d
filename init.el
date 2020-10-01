
;;;; init.el



;;; init

(defmacro init (unit &optional noerror nomessage)
  "Initialize UNIT by compile and load unit file.
UNIT is a literal symbol which may be found in `init-path'
as a file whose name is UNIT suffixed with '.el'.

If optional arguments NOERROR and NOMESSAGE is not nil,
errors and messages are suppressed.

If compile-unit is thrown during load-unit,
compile and load are performed recursively."
  (let ((name (symbol-name unit)))
    `(let ((el (locate-file ,name init-path '(".el")))
           (elc (locate-file ,name init-path '(".elc"))))
       (if (null el)
           (unless ,noerror
             (signal 'init-error
                     (list (format-message "Cannot init unit `%s'" ',unit)
                           ".el file is missing")))
         (unless (and elc (file-newer-than-file-p elc el))
           (if elc (and (delete-file elc) (setq elc nil)))
           (and (byte-compile-file el) (setq elc (byte-compile-dest-file el))))
         (if (null elc)
             (unless ,noerror
               (signal 'init-error
                       (list (format-message "Cannot init unit `%s'" ',unit)
                             ".elc file is missing")))
           (letrec ((load-unit
                     (lambda ()
                       (when (catch 'compile-unit
                               (and (load elc ,noerror ,nomessage t t) nil))
                         (and (delete-file elc) (setq elc nil))
                         (and (byte-compile-file el)
                              (setq elc (byte-compile-dest-file el)))
                         (funcall load-unit)))))
             (funcall load-unit)))))))

(defmacro init-by (file &optional noerror nomessage)
  "`init' unit by FILE.
FILE is any lisp form which returns a name of unit file.
NOERROR and NOMESSAGE suppress errors and messages."
  `(let ((unit (intern (replace-regexp-in-string
                        "\\.elc?$" ""
                        (expand-file-name ,file)))))
     (eval `(init ,unit ,',noerror ,',nomessage))))

(defmacro init-feature (feature &optional noerror nomessage)
  "`init' FEATURE.
FEATURE is a literal symbol and initialized by the unit
whose name is init-FEATURE.
NOERROR and NOMESSAGE suppress errors and messages."
  (let ((unit (intern (concat "init-" (symbol-name feature)))))
    `(init ,unit ,noerror ,nomessage)))

(defmacro init-package (package &optional noerror nomessage)
  "`init' PACKAGE.
PACKAGE is a literal symbol and initialized by the units
whose names are inst-PACKAGE and init-PACKAGE.
inst-PACKAGE should specify only installation procedure, and
any other initialization should be done by init-PACKAGE.
NOERROR and NOMESSAGE suppress errors and messages."
  (let* ((name-inst (concat "inst-" (symbol-name package)))
         (name-init (concat "init-" (symbol-name package)))
         (unit-inst (intern name-inst))
         (unit-init (intern name-init)))
    `(progn
       (init ,unit-inst ,noerror ,nomessage)
       (if (locate-file ,name-init init-path '(".el"))
           (init ,unit-init ,noerror ,nomessage)))))

(defmacro premise (unit &optional filename noerror nomessage)
  "State UNIT must be initialized.
UNIT is a literal symbol.
If UNIT is not initialized, `init' UNIT.

If FILENAME is not nil and UNIT has not been initialized,
initialize UNIT by using FILENAME.
NOERROR and NOMESSAGE suppress errors and messages.

If UNIT is newer than loading unit, throw compile-unit,
then loading unit will be compiled and loaded again."
  `(progn
     (when (not (assq ',unit init-units))
       (if ,filename
           (eval '(init-by ,filename ,noerror ,nomessage))
         (eval '(init ,unit ,noerror ,nomessage)))
       (if (not (assq ',unit init-units))
           (signal 'init-error
                   (list (format-message
                          "Premised unit `%s' has not been resolved" ',unit)))))
     (if (and load-file-name (string-match ".elc$" load-file-name))
         (let ((unit-file-name (cdr (assq ',unit init-units))))
           (if (and unit-file-name
                    (file-newer-than-file-p unit-file-name load-file-name))
               (throw 'compile-unit t))))))

(defmacro resolve (unit)
  "State UNIT is initialized.
UNIT is a literal symbol."
  `(add-to-list 'init-units (cons ',unit load-file-name)))

(defvar init-path
  (letrec ((filter (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l))
                            (cons (car l) (funcall filter p (cdr l))))
                           (t (funcall filter p (cdr l)))))))
    (apply
     'append
     (mapcar
      (lambda (d)
        (let ((p (concat user-emacs-directory d)))
          (funcall filter #'file-directory-p
                   (mapcar (lambda (f) (expand-file-name (concat p "/" f)))
                           (remove ".." (directory-files p))))))
      '("init.d/site-lisp" "init.d/lisp"))))
  "List of directories to search for files to be used for `init'.")

(defvar init-units nil "alist of initialized units and their file paths.")

(define-error 'init-error "Init error" 'error)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    `((,(concat "(\\(init\\|init-feature\\|init-package\\)\\_>"
                "[ 	]*"
                "\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t))
      ("(init-by\\_>" . font-lock-keyword-face)
      ("(\\(resolve\\|premise\\)\\_>[ 	]*\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face nil t))))))

(setq message-truncate-lines t)
(add-hook 'after-init-hook (lambda () (setq message-truncate-lines nil)))


(resolve init)


;;; units

(init custom)
(init suppression)
(init subr)
(init frame)
(init window)
(init cursor)
(init paren)
(init mark)
(init correction)
(init indent)
(init whitespace)
(init backup)
(init functions)
(init bindings)
(init mode-line)
(init risk)
(init client)
(init vc)
(init theme)
(init mouse)
(init feature)
(init package)
(init japanese)


;;; site-init

(init-by (concat user-emacs-directory "site-init.el") 'noerror)


;;; features

(init-feature eshell)
(init-feature eww)
(init-feature ediff)
(init-feature icomplete)
(init-feature ido)
(init-feature recentf)
(init-feature hippie-exp)
(init-feature dired)
(init-feature wdired)
(init-feature dired-x)
(init-feature autorevert)
(init-feature eldoc)
(init-feature org)
(init-feature flyspell)
(init-feature doc-view)
(init-feature abbrev)
(init-feature elisp-mode)
(init-feature cc-mode)
(init-feature python)
(init-feature ruby-mode)
(init-feature scheme)
(init-feature cmuscheme)


;;; user-features

(init user-feature)

(init-feature scratchb)
(init-feature fmmm)
(init-feature shifter)
(init-feature scratch)
(init-feature sdired)


;;; packages

(init-package el-get)

(init-package auto-complete)
(init-package smex)
(init-package ace-jump-mode)
(init-package visible-mark)
(init-package smartparens)
(init-package multiple-cursors)
(init-package expand-region)
(init-package anzu)
(init-package loophole)
(init-package helm)
(init-package helm-ls-git)
(init-package helm-ag)
(init-package helm-swoop)
(init-package helm-descbinds)
(init-package helm-projectile)
(init-package which-key)
(init-package volatile-highlights)
(init-package flycheck)
(init-package dired-hacks)
(init-package multi-term)
(init-package yasnippet)
(init-package yasnippet-snippets)
(init-package magit)
(init-package git-gutter-fringe)
(init-package git-modes)
(init-package projectile)
(init-package wgrep)
(init-package ag)
(init-package color-moccur)
(init-package moccur-edit)
(init-package flex-isearch)
(init-package frame-dealer)
(init-package evil)
(init-package smart-compile)
(init-package quickrun)
(init-package rainbow-delimiters)
(init-package rainbow-mode)
(init-package web-mode)
(init-package markdown-mode)
(init-package ruby-end)
(init-package inf-ruby)
(init-package robe-mode)
(init-package yaml-mode)
(init-package yatex)
(init-package ox-gfm)
(init-package ox-qmd)
(init-package smartrep)
(init-package tomorrow-theme)
(init-package color-theme-zenburn)
(init-package color-theme-solarized)
(init-package base16)
(init-package replace-colorthemes)
(init-package ddskk)


;;; site-start

(init-by (concat user-emacs-directory "site-start.el") 'noerror)
