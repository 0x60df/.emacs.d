;;; sdired.el --- Sorting utilities for dired

;;; Commentary:

;;; Code:

(require 'dired)
(require 'isearch)

(defgroup sdired nil
  "Sorting utilities for dired ."
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
      (unless (member s (list sdired-switches-for-name
                              sdired-switches-for-date
                              sdired-switches-for-size
                              sdired-switches-for-type
                              sdired-switches-for-vnum))
        (setq sdired-edited-base-switches s))))
  (sdired-refresh))

(defun sdired-sort-by (&optional key)
  "Sort dired by KEY."
  (interactive (list
                (completing-read "Key: "
                                 '("name" "date" "size" "type" "vnum"))))
  (setq sdired-base-switches
        (cond ((stringp key)
               (cond ((string-equal key "name") sdired-switches-for-name)
                     ((string-equal key "date") sdired-switches-for-date)
                     ((string-equal key "size") sdired-switches-for-size)
                     ((string-equal key "type") sdired-switches-for-type)
                     ((string-equal key "vnum") sdired-switches-for-vnum)))
              (t dired-actual-switches)))
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
Mode line displays \"by name\", \"by date\", \"by size\",
\"by type\" or \"by vnum\". If switches are other than
builtin, switches are shown literally."
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (cond ((string-equal sdired-base-switches sdired-switches-for-name)
                 "Dired by name")
                ((string-equal sdired-base-switches sdired-switches-for-date)
                 "Dired by date")
                ((string-equal sdired-base-switches sdired-switches-for-size)
                 "Dired by size")
                ((string-equal sdired-base-switches sdired-switches-for-type)
                 "Dired by type")
                ((string-equal sdired-base-switches sdired-switches-for-vnum)
                 "Dired by vnum")
                (t
                 (concat "Dired " sdired-base-switches))))
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


(provide 'sdired)

;;; sdired.el ends here
