
;;;; init.el



;;; init

(defmacro init (unit &optional noerror nomessage)
  (let ((name (symbol-name unit)))
    `(let ((el (locate-file ,name init-path '(".el")))
           (elc (locate-file ,name init-path '(".elc"))))
       (if (null el)
           (unless ,noerror
             (signal 'init-error
                     '("Cannot init unit" ".el file is missing" ,unit)))
         (unless (and elc (file-newer-than-file-p elc el))
           (if elc (and (delete-file elc) (setq elc nil)))
           (and (byte-compile-file el) (setq elc (byte-compile-dest-file el))))
         (if (null elc)
             (unless ,noerror
               (signal 'init-error
                       '("Cannot init unit" ".elc file is missing" ,unit)))
           (condition-case status
               (load elc ,noerror ,nomessage t t)
             (init-exit
              (cond ((eq (cadr status) 'recompile)
                     (and (delete-file elc) (setq elc nil))
                     (and (byte-compile-file el)
                          (setq elc (byte-compile-dest-file el)))
                     (load elc ,noerror ,nomessage t t))))))))))

(defmacro init-by (file &optional noerror nomessage)
  `(let ((unit (intern (replace-regexp-in-string
                        "\\.elc?$" ""
                        (expand-file-name ,file)))))
     (eval `(init ,unit ,',noerror ,',nomessage))))

(defmacro init-feature (feature &optional noerror nomessage)
  (let ((unit (intern (concat "init-" (symbol-name feature)))))
    `(init ,unit ,noerror ,nomessage)))

(defmacro init-package (package &optional noerror nomessage)
  (let* ((name-inst (concat "inst-" (symbol-name package)))
         (name-init (concat "init-" (symbol-name package)))
         (unit-inst (intern name-inst))
         (unit-init (intern name-init)))
    `(progn
       (init ,unit-inst ,noerror ,nomessage)
       (if (locate-file ,name-init init-path '(".el"))
           (init ,unit-init ,noerror ,nomessage)))))

(defmacro premise (unit &optional filename noerror nomessage)
  `(progn
     (if (not (assq ',unit init-units))
         (if ,filename
             (progn
               (eval '(init-by ,filename ,noerror ,nomessage))
               (if (not (assq ',unit init-units))
                   (signal 'init-error
                           (list (format "Premised unit '%s' was not resolved"
                                         ',unit)))))
           (eval '(init ,unit ,noerror ,nomessage))))
     (if (and load-file-name (string-match ".elc$" load-file-name))
         (let ((unit-file-name (cdr (assq ',unit init-units))))
           (if (and unit-file-name
                    (file-newer-than-file-p unit-file-name load-file-name))
               (signal 'init-exit
                       (list 'recompile
                             (format "Premised unit '%s' was updated"
                                     ',unit))))))))

(defmacro resolve (unit)
  `(add-to-list 'init-units (cons ',unit load-file-name)))

(defvar init-path
  (letrec ((filter (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l))))))))
    (apply
     'append
     (mapcar
      (lambda (d)
        (setq d (concat user-emacs-directory d))
        (funcall filter (lambda (f) (not (file-directory-p f)))
                 (mapcar (lambda (f) (expand-file-name (concat d "/" f)))
                         (remove ".."
                                 (directory-files d)))))
      '("init.d/site-lisp" "init.d/lisp")))))

(defvar init-units nil)

(define-error 'init-exit "Init exit" 'error)
(define-error 'init-error "Init error" 'error)

(setq message-truncate-lines t)
(add-hook 'after-init-hook (lambda () (setq message-truncate-lines nil)))


(resolve init)


;;; units

(init bars)
(init custom)
(init suppression)
(init greeting)
(init subr)
(init frame)
(init window)
(init cursor)
(init paren)
(init mark)
(init correction)
(init indent)
(init whitespace)
(init insurance)
(init tooltip)
(init mode-line)
(init functions)
(init bindings)
(init risk)
(init client)
(init vc)
(init theme)
(init mouse)
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
(init-package yasnippet)
(init-package multiple-cursors)
(init-package expand-region)
(init-package magit)
(init-package git-gutter-fringe)
(init-package git-modes)
(init-package projectile)
(init-package smex)
(init-package helm)
(init-package helm-ls-git)
(init-package helm-ag)
(init-package helm-swoop)
(init-package helm-descbinds)
(init-package helm-projectile)
(init-package dired-hacks)
(init-package evil)
(init-package anzu)
(init-package multi-term)
(init-package flycheck)
(init-package ace-jump-mode)
(init-package wgrep)
(init-package ag)
(init-package color-moccur)
(init-package moccur-edit)
(init-package flex-isearch)
(init-package visible-mark)
(init-package smart-compile)
(init-package quickrun)
(init-package smartparens)
(init-package which-key)
(init-package frame-dealer)
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
(init-package rainbow-delimiters)
(init-package rainbow-mode)
(init-package tomorrow-theme)
(init-package color-theme-zenburn)
(init-package color-theme-solarized)
(init-package base16)
(init-package replace-colorthemes)
(init-package ddskk)


;;; site-start

(init-by (concat user-emacs-directory "site-start.el") 'noerror)
