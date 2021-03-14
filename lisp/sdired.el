;;; sdired.el --- Sorting utilities for dired

;;; Commentary:

;;; Code:

(require 'dired)
(require 'isearch)

(defgroup sdired nil
  "Sorting utilities for dired."
  :group 'emacs)

(defcustom sdired-switches-for-name "-al"
  "Switches for sort by name."
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-date "-alt"
  "Switches for sort by date."
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-size "-alS"
  "Switches for sort by size."
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-type "-alX"
  "Switches for sort by type."
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-vnum "-alv"
  "Switches for sort natural order."
  :type 'string
  :group 'sdired)

(defcustom sdired-switch-for-reverse "-r"
  "Switch for reversal sort."
  :type 'string
  :group 'sdired)

(defcustom sdired-switch-for-directory-first "--group-directories-first"
  "Switch for sort where directories are listed first."
  :type 'string
  :group 'sdired)

(defvar sdired-base-switches sdired-switches-for-name "Base switches for sort.")
(make-variable-buffer-local 'sdired-base-switches)

(defvar sdired-edited-base-switches "" "Base switches edited by user.")
(make-variable-buffer-local 'sdired-edited-base-switches)

(defvar sdired-optional-switch-list '() "Optional switches for sort.")
(make-variable-buffer-local 'sdired-optional-switch-list)

(defvar sdired-base-switches-alist
  `(("name" . ,sdired-switches-for-name)
    ("date" . ,sdired-switches-for-date)
    ("size" . ,sdired-switches-for-size)
    ("type" . ,sdired-switches-for-type)
    ("vnum" . ,sdired-switches-for-vnum))
  "Alist of base switches and its label.")

(defface sdired-group '((t :inherit (dired-header italic)))
  "Face for group part of sdired."
  :group 'sdired)

(defface sdired-key '((t :inherit (dired-mark bold)))
  "Face for key part of sdired."
  :group 'sdired)

(defface sdired-active-key '((t :inherit (dired-marked bold)))
  "Face for active-key part of sdired."
  :group 'sdired)

(defface sdired-warning '((t :inherit dired-warning bold))
  "Face for warning part of sdired."
  :group 'sdired)


;;;###autoload
(defun sdired-sort (&optional arg)
  "Interface for sorting dired buffer.  Basically toggle key.
If called with prefix argument ARG, offer other features
and interactive interface."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (if arg
      (let ((current-prefix-arg nil)
            (warning-message nil)
            (commands-line
             (lambda (group &rest commands)
               (format
                "%-18s: %s"
                (propertize group 'face 'sdired-group)
                (mapconcat
                 (lambda (command-spec)
                   (let ((key (car command-spec))
                         (description (cadr command-spec))
                         (optional-switch (caddr command-spec)))
                     (format
                      "[%s] %-15s"
                      (propertize key
                                  'face
                                  (if (member optional-switch
                                              sdired-optional-switch-list)
                                      'sdired-active-key
                                    'sdired-key))
                      description)))
                 commands
                 " ")))))
        (catch 'sdired-quit-sort
          (while t
            (let* ((commands
                    `(,(funcall commands-line "Key control"
                                '("k" "select key")
                                '("t" "toggle key"))
                      ,(funcall commands-line "Custom switch"
                                '("e" "edit switches")
                                '("c" "clean switches"))
                      ,(funcall commands-line "Optional switch"
                                `("r" "reversal order"
                                  ,sdired-switch-for-reverse)
                                `("d" "directory first"
                                  ,sdired-switch-for-directory-first))
                      ,(funcall commands-line "Sort control"
                                '("s" "reset")
                                '("q" "quit"))))
                   (sequence
                    (read-key-sequence
                     (mapconcat
                      (lambda (line) (replace-regexp-in-string " +$" "" line))
                      `(,@commands
                        ,@(if (stringp warning-message) (list warning-message)))
                      "\n")))
                   (command (key-binding sequence)))
              (setq warning-message nil)
              (cond ((equal sequence "k")
                     (let ((resize-mini-windows t))
                       (call-interactively 'sdired-sort-by)))
                    ((equal sequence "t")
                     (sdired-toggle-key))
                    ((equal sequence "e")
                     (sdired-edit-switches nil))
                    ((equal sequence "c")
                     (sdired-edit-switches t))
                    ((equal sequence "r")
                     (sdired-toggle-reverse))
                    ((equal sequence "d")
                     (sdired-toggle-directory-first))
                    ((equal sequence "s")
                     (sdired-reset))
                    ((equal sequence "q")
                     (throw 'sdired-quit-sort t))
                    ((or (eq command 'universal-argument)
                         (eq command 'digit-argument)
                         (eq command 'negative-argument))
                     (setq warning-message
                           (format
                            "%-18s: [%s] %s"
                            (propertize "Warning message" 'face 'sdired-group)
                            (propertize (key-description sequence)
                                        'face 'sdired-warning)
                            (format-message "`%s' is not supported" command))))
                    (t (ignore-errors (call-interactively command))
                       (if isearch-mode (sdired--start-isearch))
                       (if (not (eq major-mode 'dired-mode))
                           (throw 'sdired-quit-sort t))))
              (window-resize (minibuffer-window)
                             (- (+ (length commands)
                                   (if warning-message 1 0))
                                (window-height (minibuffer-window))))))))
    (sdired-toggle-key)))

(defun sdired--start-isearch ()
  "Start isearch while sdired interactive interface is active."
  (add-hook 'isearch-mode-end-hook #'sdired--end-isearch)
  (recursive-edit))

(defun sdired--end-isearch ()
  "End isearch and return back to sdired interface."
  (remove-hook 'isearch-mode-end-hook #'sdired--end-isearch)
  (exit-recursive-edit))

(defun sdired-toggle-key ()
  "Toggle key of dired sort."
  (interactive)
  (setq sdired-base-switches
        (cond ((string-equal sdired-base-switches sdired-switches-for-name)
               sdired-switches-for-date)
              ((string-equal sdired-base-switches sdired-switches-for-date)
               sdired-switches-for-size)
              ((string-equal sdired-base-switches sdired-switches-for-size)
               sdired-switches-for-type)
              ((string-equal sdired-base-switches sdired-switches-for-type)
               sdired-switches-for-vnum)
              ((string-equal sdired-base-switches sdired-switches-for-vnum)
               (if (string-equal sdired-edited-base-switches "")
                   sdired-switches-for-name
                 sdired-edited-base-switches))
              ((string-equal sdired-base-switches sdired-edited-base-switches)
               sdired-switches-for-name)))
  (sdired-refresh))

(defun sdired-edit-switches (arg)
  "Edit directory switches for sort.
If ARG is non-nil, reset switches."
  (interactive "P")
  (if arg
      (setq sdired-edited-base-switches ""
            sdired-base-switches sdired-switches-for-name)
    (let ((s (read-string "ls switches (must contain -l): "
                          dired-actual-switches)))
      (setq sdired-base-switches s)
      (setq sdired-edited-base-switches
            (if (rassoc s sdired-base-switches-alist)
                ""
              s))))
  (sdired-refresh))

(defun sdired-sort-by (&optional key)
  "Sort dired by KEY."
  (interactive
   (list (completing-read "Key: " (mapcar #'car sdired-base-switches-alist))))
  (setq sdired-base-switches (if (stringp key)
                                 (cdr (assoc key sdired-base-switches-alist))
                               dired-actual-switches))
  (sdired-refresh))

(defun sdired-toggle-reverse ()
  "Sort dired with reverse option."
  (interactive)
  (if (member sdired-switch-for-reverse sdired-optional-switch-list)
      (setq sdired-optional-switch-list
            (remove sdired-switch-for-reverse sdired-optional-switch-list))
    (add-to-list 'sdired-optional-switch-list sdired-switch-for-reverse))
  (sdired-refresh))

(defun sdired-toggle-directory-first ()
  "Sort dired with directory first option."
  (interactive)
  (if (member sdired-switch-for-directory-first sdired-optional-switch-list)
      (setq sdired-optional-switch-list
            (remove sdired-switch-for-directory-first
                    sdired-optional-switch-list))
    (add-to-list 'sdired-optional-switch-list
                 sdired-switch-for-directory-first))
  (sdired-refresh))

(defun sdired-reset ()
  "Reset sort configurations."
  (interactive)
  (setq sdired-base-switches sdired-switches-for-name
        sdired-edited-base-switches ""
        sdired-optional-switch-list '())
  (sdired-refresh))

(defun sdired-set-mode-line ()
  "Set mode line display according to `sdired-base-switches'.
Mode line displays car of `sdired-base-switches-alist'.
If switches does not match with cdr of
`sdired-base-switches-alist', switches are shown literally."
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let ((entry (rassoc sdired-base-switches
                               sdired-base-switches-alist)))
            (concat "Dired " (if entry
                                 (concat "by " (car entry))
                               sdired-base-switches))))
    (force-mode-line-update)))

(defun sdired-refresh ()
  "Refresh current dired buffer according to set switches.
Call `dired-sort-other' with `sdired-base-switches',
and `sdired-optional-switch-list'.
Subsequently, call `sdired-set-mode-line'"
  (dired-sort-other
   (mapconcat 'identity
              (cons sdired-base-switches sdired-optional-switch-list)
              " "))
  (sdired-set-mode-line))

(defun sdired-update-base-switches-alist (&optional alist)
  "Update `sdired-base-switches-alist' with switches variables.
If ALIST is a list, overwrite alist by ALIST."
  (setq sdired-base-switches-alist
        (if (listp alist)
            alist
          `(("name" . ,sdired-switches-for-name)
            ("date" . ,sdired-switches-for-date)
            ("size" . ,sdired-switches-for-size)
            ("type" . ,sdired-switches-for-type)
            ("vnum" . ,sdired-switches-for-vnum)))))


(provide 'sdired)

;;; sdired.el ends here
