;;; -*- lexical-binding: t -*-
;;;; init-isearch.el


(premise init)
(premise subr)
(premise mode-line)

(defvar demi-view-isearch-mode)
(declare-function demi-view-isearch-mode load-file-name t t)

(push '(isearch-mode . 20) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'isearch
  ;; Demi view isearch mode
  (defconst demi-view-isearch-mode-bindings
    (mapcar (lambda (key-command)
              (list (car key-command) (cdr key-command)
                    (lookup-key isearch-mode-map (car key-command))))
            `((,(kbd "s") . isearch-repeat-forward)
              (,(kbd "r") . isearch-repeat-backward)))
    "List of key, binding and its original for demi-`view-isearch-mode'.")

  (defvar demi-view-isearch-mode-lighter ":V"
    "Lighter string for `demi-view-isearch-mode'.")

  (define-minor-mode demi-view-isearch-mode
    "Minor mode to view hits but not printing char."
    :global t
    :group 'user
    (if demi-view-isearch-mode
        (dolist (key-command-original demi-view-isearch-mode-bindings)
          (define-key isearch-mode-map
            (car key-command-original)
            (cadr key-command-original)))
      (dolist (key-command-original demi-view-isearch-mode-bindings)
          (define-key isearch-mode-map
            (car key-command-original)
            (caddr key-command-original)))))

  (define-key isearch-mode-map (kbd "C-]") #'demi-view-isearch-mode)
  (define-key isearch-mode-map (kbd "M-v") #'demi-view-isearch-mode)

  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (if (and demi-view-isearch-mode
                       (eq this-command #'isearch-edit-string))
                  (add-hook-for-once 'isearch-mode-hook
                                     #'demi-view-isearch-mode))
              (demi-view-isearch-mode 0)))

  ;; Lazy regexp isearch mode
  (defvar lazy-regexp-isearch-mode-lighter ":R"
    "Lighter string for `lazy-regexp-isearch-mode'.")

  (defun lazy-regexp-isearch-encode (string)
    "Encode ordinary regexp STRING to lazy regexp."
    (if (and (string-match "^\\\\(.*\\\\)$" string)
             (let ((str (replace-regexp-in-string
                         "\\\\)\\.\\*\\\\(" "" string)))
               (and (not (string-match "[^\\]\\." str))
                    (not (string-match "[^\\]\\*" str))
                    (not (string-match "[^\\]\\+" str))
                    (not (string-match "[^\\]\\?" str))
                    (not (string-match "[^\\]\\[" str))
                    (not (string-match "[^\\]\\^" str))
                    (not (string-match "[^\\]\\\\[^.*+?[^()\\]" str)))))
        (mapconcat (lambda (token)
                     (setq token (substring token 2 -2))
                     (dolist (char '("." "*" "+" "?" "[" "^" "$"))
                       (setq token (replace-regexp-in-string
                                    (concat "\\\\\\" char) char
                                    token)))
                     (setq token (replace-regexp-in-string "\\\\\\\\" "\\\\"
                                                           token))
                     (replace-regexp-in-string " " "\\ " token))
                   (split-string string "\\.\\*" t)
                   " ")
      (error "Invalid regexp string for encoding to lazy regexp: %s" string)))

  (defun lazy-regexp-isearch-decode (string)
    "Decode lazy regexp STRING to ordinary regexp."
    (let ((sl (split-string string " "))
          tl)
      (while sl
        (cond ((null (cdr sl)) (setq tl (cons (car sl) tl)))
              ((string-match "\\\\$" (car sl))
               (setq tl (cons (concat (substring (car sl) 0 -1) " ") tl))
               (while (string-match "\\\\$" (cadr sl))
                 (setcar tl
                         (concat (car tl) (substring (cadr sl) 0 -1) " "))
                 (setq sl (cdr sl)))
               (unless (string-empty-p (cadr sl))
                 (setcar tl (concat (car tl) (cadr sl)))
                 (setq sl (cdr sl))))
              (t (setq tl (cons (car sl) tl))))
        (setq sl (cdr sl)))
      (mapconcat (lambda (token)
                   (setq token (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                         token))
                   (dolist (char '("." "*" "+" "?" "[" "^" "$"))
                     (setq token (replace-regexp-in-string
                                  (concat "\\" char) (concat "\\\\" char)
                                  token)))
                   (format "\\(%s\\)" token))
                 (reverse tl)
                 ".*")))

  (advice-add 'isearch-fallback :around
              (lambda (function &rest args)
                (apply (if lazyi-regexp-isearch-mode #'ignore function) args)))

  (defun lazy-regexp-isearch-process-search-string (string message)
    "Process serch STRING and MESSAGE but with lazy regexp."
    (setq isearch-string (if lazy-regexp-isearch-mode
                             (lazy-regexp-isearch-decode
                              (concat (lazy-regexp-isearch-encode
                                       isearch-string)
                                      string))
                           (concat isearch-string string))
          isearch-message (concat isearch-message message))
    (isearch-search-and-update))

  (advice-add 'isearch-process-search-string
               :override #'lazy-regexp-isearch-process-search-string)

  (defun lazy-regexp-isearch-edit-string-advice (function &rest args)
    "Around advice for `isearch-edit-string' with lazy regexp."
    (if lazy-regexp-isearch-mode
        (let* (read-lazy-regexp
               (advice
                (lambda (fun &rest ags)
                  (setq read-lazy-regexp
                        (apply fun
                               (if (< 1 (length ags))
                                   `(,(car ags)
                                     ,(let ((initial (cadr ags)))
                                        (if (stringp (car-safe initial))
                                            (let ((string
                                                   (lazy-regexp-isearch-encode
                                                    (car initial))))
                                              (cons string
                                                    (1+ (length string))))
                                          initial))
                                     ,@(cddr ags))
                                 ags)))
                  (lazy-regexp-isearch-decode read-lazy-regexp))))
          (advice-add 'read-from-minibuffer :around advice)
          (unwind-protect
              (progn (apply function args)
                     (setq isearch-message
                           (mapconcat #'isearch-text-char-description
                                      read-lazy-regexp
                                      ""))
                     (isearch-update))
            (advice-remove 'read-from-minibuffer advice)))
      (apply function args)))

  (advice-add 'isearch-edit-string
               :around #'lazy-regexp-isearch-edit-string-advice)

  (define-minor-mode lazy-regexp-isearch-mode
    "Regexp isearch but lazy syntax."
    :group 'user
    (if lazy-regexp-isearch-mode
        (when isearch-mode
          (unless isearch-regexp (isearch-toggle-regexp))
          (let ((lazy-regexp (ignore-errors
                               (lazy-regexp-isearch-encode isearch-string))))
            (if lazy-regexp
                (progn
                  (setq isearch-message
                        (mapconcat #'isearch-text-char-description
                                   lazy-regexp ""))
                  (isearch-update))
              (setq isearch-string (lazy-regexp-isearch-decode isearch-string))
              (isearch-search-and-update))))
      (when isearch-mode
        (setq isearch-message
              (mapconcat #'isearch-text-char-description isearch-string ""))
        (isearch-update))))

  (define-key isearch-mode-map (kbd "C-/") #'lazy-regexp-isearch-mode)
  (define-key isearch-mode-map (kbd "M-w") #'lazy-regexp-isearch-mode)

  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (when (and lazy-regexp-isearch-mode
                       (eq this-command 'isearch-edit-string))
                (add-hook-for-once 'isearch-mode-hook
                                   #'lazy-regexp-isearch-mode))
              (lazy-regexp-isearch-mode 0)))

  ;; Shrinke mode line
  (let* ((buffer nil)
         (minor-mode-width nil)
         (column-number-mode-flag nil)
         (demi-view-isearch-mode-format
          '(demi-view-isearch-mode demi-view-isearch-mode-lighter))
         (lazy-regexp-isearch-mode-format
          '(lazy-regexp-isearch-mode lazy-regexp-isearch-mode-lighter))
         (hook
          (lambda ()
            (setq buffer nil)
            (setq minor-mode-width nil)
            (setq column-number-mode-flag nil)

            (mode-line-buffer-identification-shrink-mode)
            (mode-line-mode-name-shrink-mode)
            (mode-line-minor-mode-shrink-mode)
            (mode-line-vc-mode-shrink-mode)

            (setq column-number-mode-flag column-number-mode)
            (column-number-mode 0)

            (setq buffer (current-buffer))
            (if (local-variable-p 'mode-line-minor-mode-shrink-width)
                (setq minor-mode-width mode-line-minor-mode-shrink-width))
            (setq mode-line-minor-mode-shrink-width 0)

            (let ((cell (member "%]" mode-line-modes)))
              (when cell
                (setcdr cell (cons lazy-regexp-isearch-mode-format
                                   (cons (car cell) (cdr cell))))
                (setcar cell demi-view-isearch-mode-format)))))
         (end-hook
          (lambda ()
            (setq mode-line-modes
                  (delq lazy-regexp-isearch-mode-format
                        (delq demi-view-isearch-mode-format mode-line-modes)))

            (unwind-protect
                (mode-line-minor-mode-shrink-mode 0)
              (if (buffer-live-p buffer)
                  (if minor-mode-width
                      (setq mode-line-minor-mode-shrink-width minor-mode-width)
                    (with-current-buffer buffer
                      (kill-local-variable
                       'mode-line-minor-mode-shrink-width))))
              (setq buffer nil)
              (setq minor-mode-width nil))

            (if column-number-mode-flag
                (unwind-protect
                    (column-number-mode)
                  (setq column-number-mode-flag nil)))

            (mode-line-buffer-identification-shrink-mode 0)
            (mode-line-mode-name-shrink-mode 0)
            (mode-line-vc-mode-shrink-mode 0))))
    (add-hook 'isearch-mode-hook hook)
    (add-hook 'isearch-mode-end-hook end-hook))

  (modify-minor-mode-lighter 'isearch-mode
                             '((-3 "" isearch-mode)
                               (demi-view-isearch-mode
                                demi-view-isearch-mode-lighter)
                               (lazy-regexp-isearch-mode
                                lazy-regexp-isearch-mode-lighter))))


(resolve init-isearch)
