
;;;; init-company.el


(premise init)
(premise custom)
(premise subr)
(premise mode-line)
(premise inst-company)

(eval-when-compile
  (require 'company)
  (require 'company-tng))

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

(declare-function company-append-backends load-file-name t t)
(declare-function company-expand-selection-or-cycle load-file-name t t)
(declare-function company-tng-remove-text-properties load-file-name t t)
(declare-function company-expand-selection-or-cycle-reverse load-file-name t t)
(declare-function company-pseudo-tooltip-set-width load-file-name t t)
(declare-function company-pseudo-tooltip-decorate-candidate load-file-name t t)
(declare-function company-complete-inside-clean-up load-file-name t t)
(declare-function company-complete-inside-setup load-file-name t t)
(declare-function company-complete-inside-delete-suffix load-file-name t t)



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
                          company-replace-yasnippet-candidate-on-first)))

(push '(company-mode . 11) mode-line-minor-mode-priority-alist)
(push '(company-search-mode . 12) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'company



  ;;; setup

  (require 'seq)
  (require 'company-tng)

  (defface company-tooltip-yasnippet nil
    "Face used for highlighting `company-yasnippet' candidate."
    :group 'user)

  (defface company-tooltip-dabbrev-code nil
    "Face used for highlighting `company-dabbrev-code' candidate."
    :group 'user)

  (setq company-echo-delay 0.5)



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
    (when (or (not (eq command 'post-command))
              (not company-status))
      (company-preview-frontend command)))

  (defun company-pseudo-tooltip-unless-initial-inline-frontend (command)
    "`company-pseudo-tooltip-frontend', but not for initial inline."
    (unless (and (eq command 'post-command)
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
    (unless (and (eq command 'post-command)
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
   'company-capf :with 'company-dabbrev-code 'company-yasnippet)
  (company-append-backends
   'company-clang :with 'company-dabbrev-code 'company-yasnippet)
  (company-append-backends 'company-dabbrev-code :with 'company-yasnippet)
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
  
  (defvar company-completing-inside nil
    "Flag if inside completing is going on.")

  (defun company-complete-inside-setup (&rest args)
    "Setup company complete inside."
    (when (and (eq (car args) 'prefix)
               (not (company-grab-symbol)))
      (save-excursion (insert-char ?\s))
      (put-text-property (point) (+ (point) 1) 'display '(space :width 0))
      (setq company-completing-inside t)
      (add-hook-for-once 'pre-command-hook
                         (lambda ()
                           (when (null company-candidates)
                             (company-complete-inside-clean-up))))))

  (defun company-complete-inside-clean-up (&rest args)
    "Clean up company complete inside."
    (when company-completing-inside
      (delete-char 1)
      (setq company-completing-inside nil)))

  (defun company-complete-inside-delete-suffix (&rest args)
    "Delete duplicated suffix around point during complete inside."
    (when company-completing-inside
      (let* ((mid (save-excursion
                    (forward-char)
                    (point)))
             (end (save-excursion
                    (forward-char)
                    (skip-syntax-forward "w_")
                    (point)))
             (suffix (buffer-substring mid end)))
        (save-excursion
          (backward-char (length suffix))
          (when (looking-at (regexp-quote suffix))
            (delete-region mid end))))))

  (advice-add
   'company-call-backend :before #'company-complete-inside-setup)
  (add-hook 'company-completion-finished-hook
            #'company-complete-inside-delete-suffix)
  (add-hook 'company-after-completion-hook #'company-complete-inside-clean-up)



  ;;; appearance

  ;; tooltip width
  (defun company-pseudo-tooltip-set-width (&rest args)
    "Advising `company-pseudo-tooltip-frontend' to fix width."
    (if company-candidates
        (let ((width 0))
          (mapc
           (lambda (candidate)
             (let* ((annotation (company-call-backend 'annotation candidate))
                    (w (+ (string-width candidate)
                          (if annotation
                              (+ (length annotation)
                                 (if company-tooltip-align-annotations
                                     1
                                   0))
                            0))))
               (if (< width w) (setq width w))))
           company-candidates)
          (setq company-tooltip-minimum-width width)
          (setq company-tooltip-maximum-width width))))

  (advice-add 'company-pseudo-tooltip-frontend
              :before #'company-pseudo-tooltip-set-width)

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
    (let ((line (apply function
                       value annotation width selected left right args))
          (backend (get-text-property 0 'company-backend value)))
      (if (memq backend '(company-yasnippet company-dabbrev-code))
          (let ((beg (length left))
                (end (+ (length value) 1)))
            (font-lock-append-text-property
             beg end
             'face (cond
                    ((eq backend 'company-yasnippet)
                     'company-tooltip-yasnippet)
                    ((eq backend 'company-dabbrev-code)
                     'company-tooltip-dabbrev-code))
             line)))
      line))

  (advice-add 'company-fill-propertize
              :around #'company-pseudo-tooltip-decorate-candidate)



  ;;; bindings

  (define-key company-active-map (kbd "<tab>")
    #'company-expand-selection-or-cycle)
  (define-key company-active-map (kbd "<backtab>")
    #'company-expand-selection-or-cycle-reverse)

  (define-key company-active-map (kbd "C-<tab>") #'company-filter-candidates)
  (define-key company-active-map (kbd "C-v") #'company-next-page)
  (define-key company-active-map (kbd "M-v") #'company-previous-page)

  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "<return>") #'company-complete-selection))



;;; startup

(add-hook 'emacs-startup-hook #'global-company-mode)


(resolve init-company)
