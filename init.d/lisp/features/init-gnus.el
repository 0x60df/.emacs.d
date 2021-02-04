
;;;; init-gnus.el


(premise init)
(premise custom)
(premise frame)
(premise mode-line)
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
(declare-function gnus-toggle-frame-alist load-file-name t t)
(declare-function gnus-toggle-frame-alist-reverse load-file-name t t)
(declare-function gnus-toggle-fontset load-file-name t t)
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
  (setcar (cdr (assq 'gnus-topic-mode minor-mode-alist)) " tpc"))

(with-eval-after-load 'gnus-sum
  (setcar (cdr (assq 'gnus-dead-summary-mode minor-mode-alist))
          (propertize " dead" 'face 'mode-line-warning)))

(with-eval-after-load 'gnus-ml
  (setcar (cdr (assq 'gnus-mailing-list-mode minor-mode-alist)) " m-l"))



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

     (defun gnus-update-mode-lines (&rest args)
       "Update mode-lines of gnus buffers.
This can also work as after advice."
       (mapc (lambda (buffer)
               (let* ((name (buffer-name buffer))
                      (type (cond ((string-match "^\\*Group" name) 'group)
                                  ((string-match "^\\*Summary" name) 'summary)
                                  ((string-match "^\\*Article" name) 'article)
                                  ((string-match "^\\*Tree" name) 'tree))))
                 (with-current-buffer buffer
                   (cond ((eq type 'group) (gnus-group-set-mode-line))
                         (type (gnus-set-mode-line type))))))
             gnus-buffers))

     (advice-add 'gnus-update-mode-line-image-cache
                 :after #'gnus-update-mode-lines)

     (if (init-unit-p inst-yester-theme)
         (advice-add 'yester-recalc
                     :after #'gnus-update-mode-line-image-cache)))))



;;; utilities

(defcustom gnus-frame-alists nil "List of frame alist for `gnus' frame."
  :type '(repeat (alist :key-type 'symbol :value-type 'sexp))
  :group 'user)

(defcustom gnus-fontset-list nil "List of fontset for `gnus' frame."
  :type '(repeat string)
  :group 'user)

(with-eval-after-load 'gnus
  (defun gnus-toggle-frame-alist (&optional n)
    "Toggle frame alist."
    (interactive "p")
    (if gnus-frame-alists
        (let* ((parameters (frame-parameters))
               (rest (seq-drop-while
                      (lambda (alist)
                        (not (seq-every-p
                              (lambda (key-value)
                                (equal (cdr (assq (let ((key (car key-value)))
                                                    (if (eq key 'font)
                                                        'font-parameter
                                                      key))
                                                  parameters))
                                       (cdr key-value)))
                              alist)))
                      gnus-frame-alists))
               (length (length gnus-frame-alists))
               (offset (- length (length rest))))
          (modify-frame-parameters
           nil
           (nth (if rest (% (+ n offset) length) 1) gnus-frame-alists)))))

  (defun gnus-toggle-frame-alist-reverse (&optional n)
    "Toggle frame alist reverse direction."
    (interactive "p")
    (gnus-toggle-frame-alist (- n)))

  (defun gnus-toggle-fontset (&optional n)
    "Toggle frame fontset."
    (interactive "p")
    (if gnus-fontset-list
        (let* ((fontset (frame-parameter nil 'font-parameter))
               (rest (seq-drop-while
                      (lambda (f) (not (equal f fontset)))
                      gnus-fontset-list))
               (length (length gnus-fontset-list))
               (offset (- length (length rest))))
          (set-frame-parameter
           nil 'font
           (nth (if rest (% (+ n offset) length) 1) gnus-fontset-list))))))



;;; bindings

(with-eval-after-load 'gnus-group
  (define-key gnus-group-mode-map ";" #'gnus-toggle-frame-alist)
  (define-key gnus-group-mode-map "+" #'gnus-toggle-frame-alist-reverse)
  (define-key gnus-group-mode-map "\\" #'gnus-toggle-fontset))

(with-eval-after-load 'gnus-sum
  (define-key gnus-summary-mode-map ":" #'gnus-summary-expand-window)
  (define-key gnus-summary-mode-map ";" #'gnus-toggle-frame-alist)
  (define-key gnus-summary-mode-map "+" #'gnus-toggle-frame-alist-reverse)
  (define-key gnus-summary-mode-map "\\" #'gnus-toggle-fontset))

(with-eval-after-load 'gnus-art
  (define-key gnus-article-mode-map ";" #'gnus-toggle-frame-alist)
  (define-key gnus-article-mode-map "+" #'gnus-toggle-frame-alist-reverse)
  (define-key gnus-article-mode-map "\\" #'gnus-toggle-fontset))

(with-eval-after-load 'gnus-srvr
  (define-key gnus-server-mode-map ";" #'gnus-toggle-frame-alist)
  (define-key gnus-server-mode-map "+" #'gnus-toggle-frame-alist-reverse))


(resolve init-gnus)
