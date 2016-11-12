
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
           (let ((r (condition-case e
                        (load elc ,noerror ,nomessage t t)
                      (init-exit
                       (cond ((eq (cdr e) 'need-to-recompile)
                              (and (delete-file elc) (setq elc nil))
                              (and (byte-compile-file el)
                                   (setq elc (byte-compile-dest-file el)))
                              (load elc ,noerror ,nomessage t t)))))))
             (if r (add-to-list 'init-units ',unit))
             r))))))

(defmacro init-by (file &optional noerror nomessage)
  `(let ((unit (intern (replace-regexp-in-string
                        "\\.elc?$" ""
                        (expand-file-name ,file)))))
     (eval `(init ,unit ,,noerror ,,nomessage))))

(defmacro init-feature (feature &optional noerror nomessage)
  (let ((unit (intern (concat "init-" (symbol-name feature)))))
    `(init ,unit ,noerror ,nomessage)))

(defmacro premise (unit &optional filename noerror nomessage)
  (let ((name (symbol-name unit)))
    `(progn
       (if (not (memq ',unit init-units))
           (if (and ,filename
                    (or (memq (intern (replace-regexp-in-string
                                       "\\.elc?$" ""
                                       ,filename))
                              init-units)
                        (eval '(init-by ,filename ,noerror ,nomessage))))
               (add-to-list 'init-units ',unit)
             (eval '(init ,unit ,noerror ,nomessage))))
       (let ((elc (if ,filename
                      (replace-regexp-in-string "\\.elc?$\\|$" ".elc" ,filename)
                    (locate-file ,name init-path '(".elc")))))
         (if (and elc (file-newer-than-file-p elc load-file-name))
           (signal 'init-exit 'need-to-recompile))))))

(defvar init-path
  (letrec ((filter (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (funcall filter p (cdr l)))
                           (t (cons (car l) (funcall filter p (cdr l))))))))
    (apply
     'append
     (mapcar
      (lambda (d)
        (funcall filter (lambda (f) (not (file-directory-p f)))
                 (mapcar (lambda (f) (expand-file-name (concat d "/" f)))
                         (remove ".."
                                 (directory-files d)))))
      '("~/.emacs.d/init.d/site-lisp" "~/.emacs.d/init.d/lisp")))))

(defvar init-units '(init))

(define-error 'init-exit "")
(define-error 'init-error "Init error")

(setq message-truncate-lines t)
(add-hook 'after-init-hook (lambda () (setq message-truncate-lines nil)))
(custom-set-variables '(custom-file "~/.emacs.d/custom.el"))


;;; init

(init bars)
(init suppression)
(init greeting)
(init subr)
(init frame)
(init cursor)
(init paren)
(init correction)
(init indent)
(init whitespace)
(init insurance)
(init tooltip)
(init mode-line)
(init functions)
(init bindings)
(init client)
(init theme)
(init mouse)
(init japanese)


;;; site-init

(init-by "~/.emacs.d/site-init.el" 'noerror)


;;; features

(init-feature abbrev)
(init-feature doc-view)
(init-feature eshell)
(init-feature eww)
(init-feature ediff)
(init-feature icomplete)
(init-feature ido)
(init-feature org)
(init-feature cc-mode)
(init-feature python)
(init-feature ruby-mode)
(init-feature scheme)
(init-feature wdired)
(init-feature flyspell)
(init-feature recentf)


;;; el-get

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "https://raw.githubusercontent.com/"
          "dimitri/el-get/master/el-get-install.el"))
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(custom-set-variables
 '(el-get-user-package-directory "~/.emacs.d/init.d/lisp/el-get"))

(defadvice el-get-load-package-user-init-file (around catch-init-exit)
  (condition-case e
      ad-do-it
    (init-exit
     (cond ((eq (cdr e) 'need-to-recompile)
            (let* ((init-file-name (format "init-%s.el" package))
                   (package-init-file
                    (expand-file-name init-file-name
                                      el-get-user-package-directory))
                   (file-name-no-extension
                    (file-name-sans-extension package-init-file))
                   (compiled-init-file (concat file-name-no-extension ".elc"))
                   (default-directory (el-get-package-directory package)))
              (if compiled-init-file (delete-file compiled-init-file))
              (byte-compile-file package-init-file)
              (load file-name-no-extension 'noerror)))))))
(ad-activate 'el-get-load-package-user-init-file)

(el-get-bundle auto-complete)
(el-get-bundle yasnippet)
(el-get-bundle multiple-cursors)
(el-get-bundle expand-region)
(el-get-bundle magit)
(el-get-bundle git-gutter-fringe)
(el-get-bundle smex)
(el-get-bundle helm)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-ag)
(el-get-bundle helm-swoop)
(el-get-bundle projectile)
(el-get-bundle helm-projectile)
(el-get-bundle evil)
(el-get-bundle multi-term)
(el-get-bundle flycheck)
(el-get-bundle ace-jump-mode)
(el-get-bundle wgrep)
(el-get-bundle ag)
(el-get-bundle color-moccur)
(el-get-bundle moccur-edit)
(el-get-bundle smart-compile)
(el-get-bundle smartparens)
(el-get-bundle web-mode)
(el-get-bundle markdown-mode)
(el-get-bundle ruby-end)
(el-get-bundle inf-ruby)
(el-get-bundle robe-mode)
(el-get-bundle yaml-mode)
(el-get-bundle smartrep)
(el-get-bundle rainbow-delimiters)
(el-get-bundle moz-repl)
(el-get-bundle yatex)
(el-get-bundle tomorrow-theme)
(el-get-bundle color-theme-zenburn)
(el-get-bundle color-theme-solarized)
(el-get-bundle base16)
(el-get-bundle emacs-jp/replace-colorthemes)
(el-get-bundle ddskk)


;;; user-features

(mapc (lambda (e) (add-to-list 'load-path e))
      (reverse
       (letrec ((filter
                 (lambda (p l)
                   (cond ((null l) l)
                         ((funcall p (car l)) (funcall filter p (cdr l)))
                         (t (cons (car l) (funcall filter p (cdr l))))))))
         (apply 'append
                (mapcar
                 (lambda (d)
                   (funcall
                    filter
                    (lambda (f) (not (file-directory-p f)))
                    (mapcar (lambda (f) (expand-file-name (concat d "/" f)))
                            (remove ".." (directory-files d)))))
                 '("~/.emacs.d/site-lisp" "~/.emacs.d/lisp"))))))

(init-feature ox-gfm)
(init-feature ox-qmd)


;;; site-start

(init-by "~/.emacs.d/site-start.el" 'noerror)


;;; custom

(init-by custom-file 'noerror)
