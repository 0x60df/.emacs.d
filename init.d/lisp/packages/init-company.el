
;;;; init-company.el


(premise init)
(premise custom)
(premise subr)
(premise advice)
(premise bindings)
(premise mode-line)
(premise inst-company)

(eval-when-compile
  (require 'company)
  (require 'company-keywords)
  (require 'company-tng))

(declare-function company-finish "company")
(declare-function company-abort "company")
(declare-function company-pseudo-tooltip-frontend "company")
(declare-function company-preview-frontend "company")
(declare-function company-echo-metadata-frontend "company")
(declare-function company-select-next "company")
(declare-function company-select-previous "company")
(declare-function company-next-page "company")
(declare-function company-previous-page "company")
(declare-function company-call-frontends "company")
(declare-function company-call-backend "company")
(declare-function company-complete-selection "company")
(declare-function company-filter-candidates "company")
(declare-function company-grab-symbol "company")
(declare-function company--active-p "company")
(declare-function company--maybe-init-backend "company")
(declare-function company--multi-backend-adapter "company")
(declare-function company--good-prefix-p "company")
(declare-function company--prefix-str "company")
(declare-function company-calculate-candidates "company")

(declare-function company-append-backends load-file-name t t)
(declare-function company-expand-selection-or-cycle load-file-name t t)
(declare-function company-tng-remove-text-properties load-file-name t t)
(declare-function company-expand-selection-or-cycle-reverse load-file-name t t)
(declare-function company-pseudo-tooltip-set-width load-file-name t t)
(declare-function company-pseudo-tooltip-decorate-candidate load-file-name t t)
(declare-function company-complete-inside-follow-point load-file-name t t)
(declare-function company-complete-inside-finish load-file-name t t)
(declare-function company-complete-inside-after-completion load-file-name t t)
(declare-function company-complete-inside-clean-up load-file-name t t)
(declare-function company-complete-inside-check-candidates load-file-name t t)
(declare-function company-complete-inside-setup load-file-name t t)
(declare-function company-complete-inside-test-context load-file-name t t)
(declare-function company-complete-inside-delete-aux-space load-file-name t t)
(declare-function company-complete-inside-delete-suffix load-file-name t t)
(declare-function company-pseudo-tooltip-set-maximum-width-ratio
                  load-file-name t t)
(declare-function company-search-recover-fail load-file-name t t)
(declare-function company-filter-candidates-or-abort load-file-name t t)




;;; settings

(custom-set-variables
 '(company-idle-delay 0.05)
 '(company-require-match nil)
 '(company-lighter-base "C")
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tng-auto-configure nil)
 '(company-frontends '(company-tng-unless-just-beginning-frontend
                       company-pseudo-tooltip-unless-initial-inline-frontend
                       company-preview-if-just-beginning-frontend
                       company-echo-metadata-unless-initial-inline-frontend))
 '(company-transformers '(company-sort-by-length
                          company-sort-by-occurrence
                          company-replace-yasnippet-candidate-on-first))
 '(company-search-regexp-function #'company-search-words-in-any-order-regexp)
 '(company-format-margin-function nil)
 '(completion-styles '(basic emacs22)))

(push '(company-mode . 11) mode-line-minor-mode-priority-alist)
(push '(company-search-mode . 12) mode-line-minor-mode-priority-alist)



;; entry point

(define-minor-mode company-split-mode
  "Minor mode to split symbol if input is occurred on inside.")

(advice-add-for-once 'company-split-global-mode :before (lambda (&rest args)
							  (require 'company)))

(defun company-split-mode-on ()
  "Turn on `company-split-mode'."
  (if (or (eq company-global-modes t)
          (and (consp company-global-modes)
               (or (and (eq (car company-global-modes) 'not)
                        (not (memq major-mode (cdr company-global-modes))))
                   (memq major-mode company-global-modes))))
      (company-split-mode)))

(define-globalized-minor-mode company-split-global-mode company-split-mode
  company-split-mode-on
  :group 'user)



;; initialization

(with-eval-after-load 'company



  ;;; setup

  (require 'seq)
  (require 'company-tng)

  (setq company-echo-delay 0.5)
  (setq company-search-lighter '("~"
                                 (company-search-filtering "\"" "'")
                                 company-search-string
                                 (company-search-filtering "\"" "'")))

  (with-eval-after-load 'company-keywords
    (mapc (lambda (m)
            (let ((a (assq m company-keywords-alist)))
              (unless a
                (setq a (list m))
                (push a company-keywords-alist))
              (mapc (lambda (k)
                      (unless (memq k (cdr a)) (setcdr a (cons k (cdr a)))))
                    '("&optional" "&rest"))))
          '(emacs-lisp-mode lisp-interaction-mode)))



  ;;; frontend

  (defvar company-status nil "Compliting status of company.")

  (add-hook 'company-after-completion-hook (lambda (&rest args)
                                             (setq company-status nil)))

  (defun company-expand-selection-or-cycle (&optional arg)
    "Show selection or cycle selection if `company-status' is non-nil."
    (interactive "p")
    (let ((expand
           (lambda ()
             (setq company-status 'expanded)
             (company-call-frontends 'update)))
          (begin-selecting
           (lambda ()
             (setq company-status 'selecting)
             (define-key company-active-map (kbd "C-n")
               #'company-select-next)
             (define-key company-active-map (kbd "C-p")
               #'company-select-previous)))
          (cycle
           (lambda ()
             (let ((company-selection-wrap-around t)
                   (current-prefix-arg arg))
               (call-interactively 'company-select-next)))))
      (if (null company-status)
          (if (equal (nth company-selection company-candidates) company-prefix)
              (progn (funcall begin-selecting)
                     (funcall cycle))
            (funcall expand))
        (if (eq company-status 'expanded) (funcall begin-selecting))
        (funcall cycle))))

  (defun company-expand-selection-or-cycle-reverse (&optional arg)
    "`company-expand-selection-or-cycle' by reverse direction."
    (interactive "p")
    (company-expand-selection-or-cycle (if arg (- arg) -1)))

  (defun company-preview-if-just-beginning-frontend (command)
    "`company-preview-frontend', but only shown for just beginning."
    (when (or (not (memq command '(post-command unhide)))
              (not company-status))
      (company-preview-frontend command)))

  (defun company-pseudo-tooltip-unless-initial-inline-frontend (command)
    "`company-pseudo-tooltip-frontend', but not for initial inline."
    (unless (and (memq command '(post-command unhide))
                 (or (not company-status)
                     (eq company-status 'expanded)))
      (if company-tng--overlay          ; hide temporary to align tooltip.
          (let* ((ov company-tng--overlay)
                 (prefix (length company-prefix))
                 (prop (if (= prefix 0) 'after-string 'display))
                 (value (overlay-get ov prop)))
            (overlay-put ov prop nil)
            (company-pseudo-tooltip-frontend command)
            (overlay-put ov prop value)))))

  (defun company-echo-metadata-unless-initial-inline-frontend (command)
    "`company-echo-metadata-frontend', but not for initial inline."
    (unless (and (memq command '(post-command unhide))
                 (or (not company-status)
                     (eq company-status 'expanded)))
      (company-echo-metadata-frontend command)))

  (defun company-tng-unless-just-beginning-frontend (command)
    "`company-tng-frontend', but not shown for just beginning."
    (unless (and (memq command '(update pre-command))
                 (not company-status))
      (company-tng-frontend command)))

  (add-hook 'company-after-completion-hook
            (lambda (&rest args)
              (define-key company-active-map (kbd "C-n") nil)
              (define-key company-active-map (kbd "C-p") nil)))



  ;;; backend

  (defun company-append-backends (backend &optional with &rest backends)
    "Append BACKENDS to BACKEND in `company-backends'.
Find first standard backend BACKEND in `company-backends',
and append BACKENDS.
If BACKEND is not found, append it with BACKENDS.
If an optional argument WITH is non-nil, BACKENDS follows
keyword :with."
    (let ((found (or (memq backend company-backends)
                     (seq-drop-while
                      (lambda (b)
                        (not (and (listp b)
                                  (memq backend
                                        (or (memq :with (reverse b)) b)))))
                      company-backends))))
      (if found
          (when backends
            (let* ((head (car found))
                   (former (if (listp head)
                               (or (reverse (cdr (memq :with (reverse head))))
                                   head)
                             (list head)))
                   (latter (if (listp head)
                               (cdr (memq :with head))
                             nil))
                   (append (seq-difference backends `(,@former ,@latter))))
              (when append
                (setcar found `(,@former
                                ,@(unless with append)
                                ,@(if (or with latter) '(:with))
                                ,@latter
                                ,@(if with append))))))
        (add-to-list 'company-backends
                     (if backends
                         `(,@backend
                           ,@(if with `(:with))
                           ,@backends)
                       backend)
                     'append))))

  (company-append-backends
   'company-capf
   :with 'company-dabbrev-code 'company-keywords 'company-yasnippet)
  (company-append-backends
   'company-clang :with 'company-dabbrev-code 'company-yasnippet)
  (company-append-backends 'company-dabbrev-code :with 'company-yasnippet)
  (company-append-backends 'company-dabbrev :with 'company-yasnippet)
  (company-append-backends 'company-yasnippet)



  ;;; transformers

  (defun company-sort-by-length (candidates)
    "Sort candidates by length of candidates."
    (sort candidates (lambda (c1 c2)
                       (< (length c1) (length c2)))))

  (defun company-replace-yasnippet-candidate-on-first (candidates)
    "Replace yasnippet candidate if it is located at first."
    (if (and (stringp (car candidates))
             (eq (get-text-property 0 'company-backend (car candidates))
                 'company-yasnippet))
        (let ((first-candidate-other-than-yasnippet
               (seq-find (lambda (candidate)
                           (not (eq (get-text-property 0 'company-backend
                                                       candidate)
                                    'company-yasnippet)))
                         candidates)))
          (when first-candidate-other-than-yasnippet
            (setcar (memq first-candidate-other-than-yasnippet candidates)
                    (car candidates))
            (setcar candidates first-candidate-other-than-yasnippet))
          candidates)
      candidates))



  ;;; complete-inside

  (defvar company-complete-inside-context nil
    "Context information for company-complete-inside.
This alist contains line number, prefix and suffix after
complete-inside is started.")

  (defvar company-complete-inside-point nil
    "Point integer for company-complete-inside.")

  (defun company-complete-inside-setup ()
    "Setup company complete inside."
    (unless (company--active-p) (company-complete-inside-clean-up))
    (when (and company-split-mode
               (null company-complete-inside-context)
               (memq this-command company-begin-commands)
               (not (equal (this-command-keys) " "))
               (not (company-grab-symbol)))
      (setq company-complete-inside-context
            `((line . ,(line-number-at-pos))
              (prefix . ,(buffer-substring
                          (save-excursion (skip-syntax-backward "w_")
                                          (point))
                          (point)))
              (suffix . ,(buffer-substring
                          (point)
                          (save-excursion (skip-syntax-forward "w_")
                                          (point))))))
      (save-excursion (insert-char ?\s))
      (add-hook-for-once
       'post-command-hook
       (lambda ()
         (if (not (company-complete-inside-check-candidates))
             (progn
               (company-complete-inside-delete-aux-space)
               (company-complete-inside-clean-up))
           (company-complete-inside-follow-point)
           (add-hook-for-once
            'pre-command-hook
            (lambda ()
              (if (not (company--active-p))
                  (progn
                    (company-complete-inside-delete-aux-space)
                    (company-complete-inside-clean-up))
                (add-hook-for-once
                 'company-completion-finished-hook
                 #'company-complete-inside-finish)
                (add-hook-for-once
                 'company-after-completion-hook
                 #'company-complete-inside-after-completion)))))))))

  (defun company-complete-inside-finish (&rest args)
    "Function for hook `company-completion-finished-hook'."
    (company-complete-inside-delete-suffix)
    (setq company-complete-inside-point (point)))

  (defun company-complete-inside-after-completion (&rest args)
    "Function for hook `company-after-completion-hook'."
    (let ((point (point)))
      (if company-complete-inside-point
          (save-excursion
            (goto-char company-complete-inside-point)
            (company-complete-inside-delete-aux-space)))
      (company-complete-inside-delete-aux-space)
      (if (and (eql (+ (point) 1) point)
               (eql (point) company-complete-inside-point))
          (ignore-errors (forward-char))))
    (company-complete-inside-clean-up))

  (defun company-complete-inside-follow-point ()
    "Follow `company-point' during company-complete-inside is active."
    (when company-complete-inside-context
      (setq company-complete-inside-point (point))
      (add-hook-for-once 'pre-command-hook
                          #'company-complete-inside-follow-point)))

  (defun company-complete-inside-clean-up ()
    "Clean up complete inside."
    (interactive)
    (setq company-complete-inside-context nil)
    (setq company-complete-inside-point nil)
    (remove-hook-for-once 'pre-command-hook
                          #'company-complete-inside-follow-point)
    (remove-hook-for-once 'company-completion-finished-hook
                          #'company-complete-inside-finish)
    (remove-hook-for-once 'company-after-completion-hook
                          #'company-complete-inside-after-completion))

  (defun company-complete-inside-check-candidates ()
    "Check and return candidates without modifying company state."
    (let (prefix c company-prefix company-backend)
      (cl-dolist (backend (if company-backend
                              (list company-backend)
                            company-backends))
        (setq prefix
              (if (or (symbolp backend)
                      (functionp backend))
                  (when (company--maybe-init-backend backend)
                    (let ((company-backend backend))
                      (company-call-backend 'prefix)))
                (company--multi-backend-adapter backend 'prefix)))
        (when prefix
          (when (company--good-prefix-p prefix)
            (let ((ignore-case (company-call-backend 'ignore-case)))
              (setq company-prefix (company--prefix-str prefix)
                    company-backend backend
                    c (company-calculate-candidates
                       company-prefix ignore-case))))
          (cl-return c)))))

  (defun company-complete-inside-test-context ()
    "Test if current context is same as saved one."
    (and (eql (char-after) ?\s)
         (eql (line-number-at-pos)
              (cdr (assq 'line company-complete-inside-context)))
         (save-excursion
           (skip-syntax-backward "w_")
           (looking-at
            (regexp-quote
             (cdr (assq 'prefix company-complete-inside-context)))))
         (ignore-errors
           (equal (buffer-substring
                   (save-excursion (forward-char)
                                   (point))
                   (save-excursion (forward-char) (skip-syntax-forward "w_")
                                   (point)))
                  (cdr (assq 'suffix company-complete-inside-context))))))

  (defun company-complete-inside-delete-aux-space ()
    "Delete aux space for company complete inside."
    (if (company-complete-inside-test-context)
        (delete-char 1)))

  (defun company-complete-inside-delete-suffix ()
    "Delete duplicated suffix around point during complete inside.
If suffix does not match, delete aux space."
    (if (company-complete-inside-test-context)
        (ignore-errors
          (let* ((mid (save-excursion (forward-char)
                                      (point)))
                 (end (save-excursion (forward-char) (skip-syntax-forward "w_")
                                      (point)))
                 (suffix (buffer-substring mid end)))
            (save-excursion
              (backward-char (length suffix))
              (if (looking-at (regexp-quote (concat suffix " ")))
                  (delete-region (- mid 1) end)))))))

  (add-hook 'pre-command-hook #'company-complete-inside-setup)



  ;;; mode-line lighter
  (defconst company-lighter-transform-alist
    '(("yasnippet" . "yas")
      ("dabbrev-code" . "dabrv-c"))
    "Alist of transforming company lighter backend.")

  (advice-add
   'company--group-lighter
   :filter-return (lambda (return)
                    (mapc
                     (lambda (a)
                       (setq return
                             (replace-regexp-in-string (car a) (cdr a) return)))
                     (mapcar (lambda (a)
                               `(,(regexp-quote (format "<%s>" (car a)))
                                 . ,(format "<%s>" (cdr a))))
                             company-lighter-transform-alist))
                    return))

  (defun company--sole-lighter (backend)
    "Return lighter string for company BACKEND."
    (let ((name (replace-regexp-in-string "company-\\|-company" ""
                                          (symbol-name backend))))
      (mapc
       (lambda (a)
         (setq name
               (replace-regexp-in-string (regexp-quote (car a)) (cdr a) name)))
       company-lighter-transform-alist)
      (format "%s-{%s}" company-lighter-base name)))

  (let ((cell (cdddr (cadr (cadr (cadr company-lighter))))))
    (when (equal (car cell) '(symbol-name company-backend))
      (setcar cell '(company--sole-lighter company-backend))))



  ;;; appearance

  ;; tooltip width
  (defvar company-pseudo-tooltip-maximum-width-ratio 0.6
    "Ratio of maximum width of company tooltip against frame width.
If width of candidate plus annotation exceeds this value,
annotation section is truncated as this ratio.
Candidates are never truncated, thus final width of tooltip
can be more than this value.")

  (defun company-pseudo-tooltip-set-width (&rest args)
    "Advising `company-pseudo-tooltip-frontend' to fix width."
    (if company-candidates
        (let ((maximum (round (* company-pseudo-tooltip-maximum-width-ratio
                                 (frame-width))))
              (width 0))
          (mapc
           (lambda (candidate)
             (if (< width maximum)
                 (let* ((annotation (company-call-backend
                                     'annotation candidate))
                        (w (+ (string-width candidate)
                              (if annotation
                                  (+ (string-width annotation)
                                     (if company-tooltip-align-annotations
                                         1
                                       0))
                                0))))
                   (cond ((< maximum w) (setq width maximum))
                         ((< width w) (setq width w))))
               (let ((w (string-width candidate)))
                 (if (< width w) (setq width w)))))
           company-candidates)
          (setq company-tooltip-minimum-width width)
          (setq company-tooltip-maximum-width width))))

  ;; (advice-add 'company-pseudo-tooltip-frontend
  ;;             :before #'company-pseudo-tooltip-set-width)

  (defun company-pseudo-tooltip-set-maximum-width-ratio (ratio)
    "Read RATIO and set `company-tooltip-maximum-width-ratio'."
    (interactive (list (read-number
                        "Ratio: "
                        company-pseudo-tooltip-maximum-width-ratio)))
    (setq company-pseudo-tooltip-maximum-width-ratio ratio))

  ;; tng text properties
  (defun company-tng-remove-text-properties (args)
    "Advice `overlay-put' in `company-tng-frontend' to strip preview."
    (let ((ov (car args))
          (prop (cadr args))
          (value (caddr args)))
      (if (and company-tng--overlay
               (eq ov company-tng--overlay)
               (eq prop 'display)
               (stringp value))
          (list ov prop (substring-no-properties value))
        args)))

  (add-hook 'company-completion-started-hook
             (lambda (&rest args)
               (advice-add 'overlay-put
                            :filter-args #'company-tng-remove-text-properties)))
  (add-hook 'company-after-completion-hook
            (lambda (&rest args)
              (advice-remove 'overlay-put
                             #'company-tng-remove-text-properties)))

  ;; tooltip candidate decoration
  (defun company-pseudo-tooltip-decorate-candidate
      (function value annotation width selected left right &rest args)
    "Advising `company-fill-propertize' to decorate candidate."
    (let* ((line (apply function
                        value annotation width selected left right args))
           (kind (company-call-backend 'kind value))
           (face (cond ((null kind) 'default)
                       ((eq kind 'text) 'default)
                       ((eq kind 'keyword) 'font-lock-keyword-face)
                       ((eq kind 'function) 'font-lock-function-name-face)
                       ((eq kind 'variable) 'font-lock-variable-name-face)
                       ((eq kind 'folder) 'dired-directory)
                       ((eq kind 'file) 'default)
                       ((eq kind 'module) 'font-lock-type-face)
                       ((eq kind 'class) 'font-lock-type-face)
                       ((eq kind 'method) 'font-lock-function-name-face)
                       ((eq kind 'snippet) 'error)
                       ((eq kind 'color) 'success)
                       (t 'shadow))))
      (if face
          (let ((beg (length left))
                (end (+ (length value) 1)))
            (font-lock-append-text-property beg end 'face face line)))
      line))

  (advice-add 'company-fill-propertize
              :around #'company-pseudo-tooltip-decorate-candidate)



  ;; cursor color for search mode
  (defvar company-search-failed nil
    "State if company search is failed.")

  (defvar company-search-failed-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "DEL") #'company-search-recover-fail)
      map)
    "Keymap for company search is failed.")

  (defvar company-user-emulation-alist
    `((company-search-failed . ,company-search-failed-map))
    "Alist of emulation map for company user customization.")

  (defvar company-standard-cursor-color nil
    "Temporary store for standard cursor color.")

  (defcustom company-search-cursor-color (face-attribute 'cursor :background)
    "Cursor color for `company-search-mode'."
    :group 'user
    :type 'color)
  (defvar company-search-cursor-color)

  (defcustom company-search-fail-cursor-color
    (face-attribute 'cursor :background)
    "Cursor color for `company-search-mode' is failed to match."
    :group 'user
    :type 'color)
  (defvar company-search-fail-cursor-color)

  (defun company-search-recover-fail ()
    "Recover from failed state of company search."
    (interactive)
    (setq company-search-failed nil)
    (if company-search-mode
        (set-frame-parameter nil 'cursor-color company-search-cursor-color)
      (when company-standard-cursor-color
        (set-frame-parameter nil 'cursor-color company-standard-cursor-color))))

  (add-hook 'company-after-completion-hook (lambda (&rest args)
                                             (company-search-recover-fail)))

  (add-hook 'company-search-mode-hook
            (lambda ()
              (if (not company-search-mode)
                  (when company-standard-cursor-color
                    (set-frame-parameter nil 'cursor-color
                                         company-standard-cursor-color))
                (setq company-standard-cursor-color
                      (frame-parameter nil 'cursor-color))
                (set-frame-parameter nil 'cursor-color
                                     company-search-cursor-color))))

  (advice-add
   'company--search-update-predicate
   :around (lambda (company--search-update-predicate &rest args)
             (let ((error t))
               (unwind-protect
                   (prog1 (apply company--search-update-predicate args)
                     (setq error nil))
                 (if (not error)
                     (set-frame-parameter nil 'cursor-color
                                          company-search-cursor-color)
                   (set-frame-parameter nil 'cursor-color
                                        company-search-fail-cursor-color)
                   (setq company-search-failed t)
                   (setq emulation-mode-map-alists
                         (cons 'company-user-emulation-alist
                               (delq 'company-user-emulation-alist
                                     emulation-mode-map-alists))))))))

  (advice-add
   'company-search-printing-char
   :around (lambda (company-search-printing-char &rest args)
             (unless company-search-failed
               (apply company-search-printing-char args))))



  ;;; utilities

  (defun company-filter-candidates-or-abort-and-hippie-expand ()
    "`company-filter-candidates' if selecting, else `company-abort'.
After abort, call `hippie-expand'."
    (interactive)
    (cond ((eq company-status 'selecting) (company-filter-candidates))
          ((eq company-status 'expanded)
           (company-finish (car company-candidates)))
          (t (company-abort)
             (hippie-expand current-prefix-arg))))



  ;;; bindings

  (define-key company-active-map (kbd "<tab>")
    #'company-expand-selection-or-cycle)
  (define-key company-active-map (kbd "<backtab>")
    #'company-expand-selection-or-cycle-reverse)

  (define-key company-active-map (kbd "C-<tab>")
    #'company-filter-candidates-or-abort)
  (define-key company-active-map (kbd "C-v") #'company-next-page)
  (define-key company-active-map (kbd "M-v") #'company-previous-page)

  (overriding-set-key (kbd "C-c <tab>")
                      #'company-pseudo-tooltip-set-maximum-width-ratio)

  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "<return>") #'company-complete-selection))



;;; startup

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'company-split-global-mode)


(resolve init-company)
