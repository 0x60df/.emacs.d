
;;;; init-gnus.el


(premise init)
(premise custom)
(premise frame)
(premise mode-line)
(premise font)
(premise init-auth-source)
(premise init-flyspell)

(eval-when-compile
  (require 'nnfolder)
  (require 'gnus-srvr)
  (require 'gnus-agent))

(defvar gnus-tmp-group)

(declare-function gnus-group-get-parameter "gnus")
(declare-function gnus-summary-expand-window "gnus-sum")
(declare-function mm-image-load-path "mm-util")
(declare-function gnus-group-sort-groups "gnus-group")

(lazy-autoload 'gnus-group-set-mode-line "gnus-group")
(lazy-autoload 'gnus-set-mode-line "gnus-sum")
(lazy-autoload 'gnus-topic-mode "gnus-topic")

(declare-function gnus-update-mode-line-image-cache load-file-name t t)
(declare-function gnus-update-mode-lines load-file-name t t)
(declare-function gnus-agent-store-mode load-file-name t t)
(declare-function gnus-agent-shrink-mode-line-string load-file-name t t)


;;; settings

(defconst gnus-root-directory (concat user-emacs-directory "gnus/")
  "Gnus root directory which will be drived to related variables.")

(with-eval-after-load 'gnus
  (defun gnus-user-format-function-g (headers)
    "User format function for gnus line format displaying comment."
    (let ((comment (gnus-group-get-parameter gnus-tmp-group 'comment)))
      (if comment
          (format "%s[%s]" comment gnus-tmp-group)
        gnus-tmp-group))))

(custom-set-variables
 '(gnus-home-directory gnus-root-directory)
 '(gnus-directory (concat gnus-root-directory "News/"))
 '(message-directory (concat gnus-root-directory "Mail/"))
 '(nntp-authinfo-file (cadr (assq :source auth-sources)) nil (auth-source))

 '(message-signature-file (concat gnus-root-directory ".signature"))

 '(gnus-fetch-old-headers 'some)
 '(gnus-group-sort-function '(gnus-group-sort-by-alphabet
                              gnus-group-sort-by-rank))
 '(gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))

 '(gnus-face-3 'underline)
 '(gnus-face-4 'shadow)

 '(gnus-group-line-format "%M\ %S\ %p\ %6y:%B%ug\n")
 '(gnus-topic-line-format "%i%4{+ %n -- %A%}%v\n")

 '(gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references))

(with-eval-after-load 'nnfolder
  (setq nnfolder-directory (concat gnus-root-directory "Mail/abrchive/"))
  (setq nnfolder-active-file (concat nnfolder-directory "active")))



;;; hooks

(with-eval-after-load 'gnus-group
  (add-hook 'gnus-group-mode-hook
            (lambda ()
              (call-interactively #'gnus-group-sort-groups)
              (gnus-topic-mode))))

(with-eval-after-load 'gnus-cus
  (add-hook 'gnus-custom-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))

(with-eval-after-load 'message
  (add-hook 'message-mode-hook #'flyspell-mode))



;;; mode-line

(push '(gnus-dead-summary-mode . 1) mode-line-minor-mode-priority-alist)
(push '(gnus-agent-group-mode . 2) mode-line-minor-mode-priority-alist)
(push '(gnus-agent-summary-mode . 3) mode-line-minor-mode-priority-alist)
(push '(gnus-topic-mode . 4) mode-line-minor-mode-priority-alist)
(push '(gnus-mailing-list-mode . 5) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'gnus-agent
  (setcar (cdr gnus-agent-mode-status) " plg")

  (defvar gnus-agent-mode-list nil "List of descendant of `gnus-agent-mode'.")

  (defun gnus-agent-store-mode (&rest args)
    "Advising `gnus-agent-mode' to list descendant of `gnus-agent-mode'."
    (let* ((buffer (progn (string-match "^gnus-\\(.*\\)-mode$"
				        (symbol-name major-mode))
			  (match-string 1 (symbol-name major-mode))))
	   (mode (intern (format "gnus-agent-%s-mode" buffer))))
      (add-to-list 'gnus-agent-mode-list mode)))

  (advice-add 'gnus-agent-mode :after #'gnus-agent-store-mode)

  (defun gnus-agent-shrink-mode-line-string (&rest args)
    "Advising `gnus-agent-toggle-plugged' to shrink mode line string of."
    (cond ((equal (cadr gnus-agent-mode-status) " Plugged")
           (setcar (cdr gnus-agent-mode-status) " plg"))
          ((equal (cadr gnus-agent-mode-status) " Unplugged")
           (setcar (cdr gnus-agent-mode-status)
                   (propertize " uplg" 'face 'mode-line-warning))))
    (mapc (lambda (mode)
            (let ((cell (assq mode minor-mode-alist)))
              (if cell
                  (setcar (cdr cell) (cadr gnus-agent-mode-status)))))
          gnus-agent-mode-list))

  (advice-add 'gnus-agent-toggle-plugged
              :after #'gnus-agent-shrink-mode-line-string))

(with-eval-after-load 'gnus-topic
  (modify-minor-mode-lighter 'gnus-topic-mode " tpc"))

(with-eval-after-load 'gnus-sum
  (modify-minor-mode-lighter
   'gnus-dead-summary-mode (propertize " dead" 'face 'mode-line-warning)))

(with-eval-after-load 'gnus-ml
  (modify-minor-mode-lighter 'gnus-mailing-list-mode " m-l"))



;;; patch for images

(defconst gnus-mode-line-image-file "gnus-pointer.xpm"
  "Name of image file for gnus mode line.")

(defcustom gnus-mode-line-image-color "#0000ff"
  "Color of gnus mode line image."
  :type 'color
  :group 'user)

(let* ((destination (concat gnus-root-directory gnus-mode-line-image-file)))
  (unless (file-readable-p destination)
    (require 'mm-util)
    (let* ((load-path (append (mm-image-load-path) load-path))
           (image (find-image `((:type xpm :file ,gnus-mode-line-image-file))))
           (source (plist-get (cdr image) :file)))
      (if source
          (with-temp-file destination
            (insert-file-contents source)
            (re-search-forward "\\(\\. c #0000ff\\)" nil t)
            (replace-match "\\1 s thing" t))
        (make-empty-file destination)))))

(defvar gnus-mode-line-image-cache)
(unless (version< emacs-version "28")
  (defvar gnus-mode-line-image-cache nil "Chace of gnus mode line image."))

(add-hook
 'after-init-hook
 (lambda ()
   (with-eval-after-load 'gnus
     (defun gnus-update-mode-line-image-cache (&rest args)
       "Update `gnus-mode-line-image-cache' accoding to current context.
This can also work as after advice."
       (let ((destination (concat gnus-root-directory
                                  gnus-mode-line-image-file)))
         (setq gnus-mode-line-image-cache
               (if (and (file-readable-p destination)
                        (not (zerop (file-attribute-size
                                     (file-attributes destination)))))
                   (let ((load-path (list gnus-root-directory)))
                     (find-image
                      `((:type xpm :file ,gnus-mode-line-image-file
                               :ascent center
                               :color-symbols
                               (("thing" .
                                 ,gnus-mode-line-image-color))))))))))

     (gnus-update-mode-line-image-cache)

     (cond ((version< emacs-version "28")
            (defun gnus-update-mode-lines (&rest args)
              "Update mode-lines of gnus buffers.
This can also work as after advice."
              (mapc (lambda (buffer)
                      (let* ((name (buffer-name buffer))
                             (type
                              (cond ((string-match "^\\*Group" name) 'group)
                                    ((string-match "^\\*Summary" name) 'summary)
                                    ((string-match "^\\*Article" name) 'article)
                                    ((string-match "^\\*Tree" name) 'tree))))
                        (with-current-buffer buffer
                          (cond ((eq type 'group) (gnus-group-set-mode-line))
                                (type (gnus-set-mode-line type))))))
                    gnus-buffers))

            (advice-add 'gnus-update-mode-line-image-cache
                        :after #'gnus-update-mode-lines))
           (t
            (advice-add
             'gnus-mode-line-buffer-identification
             :around
             (lambda (original &rest args)
               (if gnus-mode-line-image-cache
                   (let ((advice (lambda (return) gnus-mode-line-image-cache)))
                     (advice-add 'find-image :filter-return advice)
                     (unwind-protect
                         (apply original args)
                       (advice-remove 'find-image advice)))
                 (apply original args))))))

     (if (init-unit-p inst-yester-theme)
         (advice-add 'yester-recalc
                     :after #'gnus-update-mode-line-image-cache)))))



;;; bindings

(with-eval-after-load 'gnus-group
  (define-key gnus-group-mode-map ";" #'switch-frame-alist)
  (define-key gnus-group-mode-map "+" #'switch-frame-alist-reverse)
  (define-key gnus-group-mode-map "\\" #'switch-frame-fontset))

(with-eval-after-load 'gnus-sum
  (define-key gnus-summary-mode-map ":" #'gnus-summary-expand-window)
  (define-key gnus-summary-mode-map ";" #'switch-frame-alist)
  (define-key gnus-summary-mode-map "+" #'switch-frame-alist-reverse)
  (define-key gnus-summary-mode-map "\\" #'switch-frame-fontset))

(with-eval-after-load 'gnus-art
  (define-key gnus-article-mode-map ";" #'switch-frame-alist)
  (define-key gnus-article-mode-map "+" #'switch-frame-alist-reverse)
  (define-key gnus-article-mode-map "\\" #'switch-frame-fontset))

(with-eval-after-load 'gnus-srvr
  (define-key gnus-server-mode-map ";" #'switch-frame-alist)
  (define-key gnus-server-mode-map "+" #'switch-frame-alist-reverse))


(resolve init-gnus)
