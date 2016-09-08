
;;;; init.el


(setq message-truncate-lines t)
(add-hook 'after-init-hook (lambda () (setq message-truncate-lines nil)))
(custom-set-variables '(custom-file "~/.emacs.d/custom.el"))

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

(defmacro init (unit &optional noerror nomessage nosuffix must-suffix)
  `(let* ((name (symbol-name ',unit))
          (el (locate-file name init-path '(".el")))
          (elc (locate-file name init-path '(".elc"))))
     (when el
       (unless (and elc (file-newer-than-file-p elc el))
         (if elc (delete-file elc))
         (byte-compile-file el)
         (setq elc (byte-compile-dest-file el)))
       (load elc ,noerror ,nomessage ,nosuffix ,must-suffix))))

(defun init-by (file)
  (let ((unit (make-symbol (replace-regexp-in-string "\\.el$" "" file))))
    (eval `(init ,unit))))

(defmacro init-feature (feature)
  (let ((unit (make-symbol (concat "init-" (symbol-name feature)))))
     `(init ,unit)))


;;; init

(init bars)
(init suppression)
(init greeting)
(init cursor)
(init paren)
(init indent)
(init whitespace)
(init insurance)
(init tooltip)
(init mode-line)
(init bindings)
(init functions)
(init theme)
(init mouse)
(init japanese)


;;; site-init

(init-by "~/.emacs.d/site-init.el")


;;; features

(init-feature abbrev)
(init-feature doc-view)
(init-feature eshell)
(init-feature eww)
(init-feature icomplete)
(init-feature ido)
(init-feature org)
(init-feature cc-mode)
(init-feature python)
(init-feature ruby-mode)
(init-feature scheme)
(init-feature wdired)


;;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       (concat
	"https://raw.githubusercontent.com/"
	"dimitri/el-get/master/el-get-install.el"))
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(custom-set-variables
 '(el-get-user-package-directory "~/.emacs.d/init.d/lisp/el-get"))

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
(el-get-bundle flycheck)
(el-get-bundle ace-jump-mode)
(el-get-bundle ace-isearch)
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
(el-get-bundle yatex)
(el-get-bundle smartrep)
(el-get-bundle rainbow-delimiters)
(el-get-bundle tomorrow-theme)
(el-get-bundle color-theme-zenburn)
(el-get-bundle solarized-theme)
(el-get-bundle base16)
(el-get-bundle emacs-jp/replace-colorthemes)


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


;;; custom

(init-by custom-file)


;;; site-start

(init-by "~/.emacs.d/site-start.el")
