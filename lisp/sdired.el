;;; sdired.el --- dired sorting utilities

;;; code:

(require 'dired)
(require 'isearch)

(defgroup sdired nil
  "dired sorting utilities"
  :group 'emacs)

(defcustom sdired-switches-for-name "-al"
  "Switches for sort by name"
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-date "-alt"
  "Switches for sort by date"
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-size "-alS"
  "Switches for sort by size"
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-type "-alX"
  "Switches for sort by type"
  :type 'string
  :group 'sdired)

(defcustom sdired-switches-for-vnum "-alv"
  "Switch for natural sort"
  :type 'string
  :group 'sdired)

(defcustom sdired-switch-for-reverse "-r"
  "Switch for reversal sort"
  :type 'string
  :group 'sdired)

(defcustom sdired-switch-for-directory-first "--group-directories-first"
  "Switch for sort where directories are listed first"
  :type 'string
  :group 'sdired)

(defvar sdired-base-switches sdired-switches-for-name "Base switches for sort")
(make-variable-buffer-local 'sdired-base-switches)
(defvar sdired-other-base-switches "" "Base switches other than builtin")
(make-variable-buffer-local 'sdired-other-base-switches)
(defvar sdired-optional-switches '() "Optional switches for sort")
(make-variable-buffer-local 'sdired-optional-switches)

;;;###autoload
(defun sdired-sort (&optional arg)
  "Interface for sort. Basically toggle key.
 if called with prefix argument, offer other features."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (if arg
      (let ((current-prefix-arg nil)
            (warning-message nil))
        (catch 'sdired-quit-sort
          (while t
            (let* ((s (read-key-sequence
                       (concat
                        (propertize "Key control       "
                                    'face '(dired-ignored italic))
                        ": "
                        "["
                        (propertize "k" 'face '(dired-mark bold))
                        "] select key      ["
                        (propertize "t"  'face '(dired-mark bold))
                        "] toggle key\n"
                        (propertize "Custom switch     " 'face
                                    '(dired-ignored italic))
                        ": "
                        "["
                        (propertize "e"  'face '(dired-mark bold))
                        "] edit switches   ["
                        (propertize "c"  'face '(dired-mark bold))
                        "] clean switches\n"
                        (propertize "Optional switch   " 'face
                                    '(dired-ignored italic))
                        ": "
                        "["
                        (propertize
                         "r"  'face
                         (list (if (member sdired-switch-for-reverse
                                           sdired-optional-switches)
                                   'dired-marked
                                 'dired-mark)
                               'bold))
                        "] reversal order  ["
                        (propertize
                         "d"  'face
                         (list (if (member sdired-switch-for-directory-first
                                           sdired-optional-switches)
                                   'dired-marked
                                 'dired-mark)
                               'bold))
                        "] directory first\n"
                        (propertize "Sort control      "
                                    'face '(dired-ignored italic))
                        ": "
                        "["
                        (propertize "s"  'face '(dired-mark bold))
                        "] reset           ["
                        (propertize "q"  'face '(dired-mark bold))
                        "] quit"
                        (if (stringp warning-message)
                            warning-message))))
                   (c (key-binding s)))
              (setq warning-message nil)
              (cond ((equal s "k")
                     (let ((resize-mini-windows t))
                       (call-interactively 'sdired-sort-by)))
                    ((equal s "t")
                     (sdired-toggle-key))
                    ((equal s "e")
                     (sdired-edit-switches nil))
                    ((equal s "c")
                     (sdired-edit-switches t))
                    ((equal s "r")
                     (sdired-toggle-reverse))
                    ((equal s "d")
                     (sdired-toggle-directory-first))
                    ((equal s "s")
                     (sdired-reset))
                    ((equal s "q")
                     (throw 'sdired-quit-sort t))
                    ((or (eq c 'universal-argument)
                         (eq c 'digit-argument)
                         (eq c 'negative-argument))
                     (setq warning-message
                           (concat "\n"
                                   (propertize "Warning message   "
                                               'face '(dired-ignored italic))
                                   ": "
                                   "["
                                   (propertize (key-description s)
                                               'face '(dired-warning bold))
                                   "] "
                                   (format-message "`%s' is not supported" c))))
                    (t (ignore-errors (call-interactively c))
                       (if isearch-mode (sdired--start-isearch))
                       (if (not (eq major-mode 'dired-mode))
                           (throw 'sdired-quit-sort t))))
              (window-resize (minibuffer-window)
                             (- (if warning-message 5 4)
                                (window-height (minibuffer-window))))))))
    (sdired-toggle-key)))

(defun sdired--start-isearch ()
  "Start isearch while interactive sdired sort interface is active."
  (add-hook 'isearch-mode-end-hook #'sdired--end-isearch)
  (recursive-edit))

(defun sdired--end-isearch ()
  "End isearch while and return back to sdired interface."
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
               (if (string-equal sdired-other-base-switches "")
                   sdired-switches-for-name
                 sdired-other-base-switches))
              ((string-equal sdired-base-switches sdired-other-base-switches)
               sdired-switches-for-name)))
  (sdired-refresh))

(defun sdired-edit-switches (arg)
  "Edit directory switches for sort."
  (interactive "P")
  (if arg
      (setq sdired-other-base-switches ""
            sdired-base-switches sdired-switches-for-name)
    (let ((s (read-string "ls switches (must contain -l): "
                          dired-actual-switches)))
      (setq sdired-base-switches s)
      (unless (or (string-equal s sdired-switches-for-name)
                  (string-equal s sdired-switches-for-date)
                  (string-equal s sdired-switches-for-size)
                  (string-equal s sdired-switches-for-type)
                  (string-equal s sdired-switches-for-vnum))
        (setq sdired-other-base-switches s))))
  (sdired-refresh))

(defun sdired-sort-by (&optional key)
  "Sort dired by KEY.
If called with prefix argument, sort switches can be edit manually"
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
  "Sort dired with reverse option"
  (interactive)
  (if (member sdired-switch-for-reverse sdired-optional-switches)
      (setq sdired-optional-switches
            (remove sdired-switch-for-reverse sdired-optional-switches))
    (add-to-list 'sdired-optional-switches sdired-switch-for-reverse))
  (sdired-refresh))

(defun sdired-toggle-directory-first ()
  "Sort dired with directory first option"
  (interactive)
  (if (member sdired-switch-for-directory-first sdired-optional-switches)
      (setq sdired-optional-switches
            (remove sdired-switch-for-directory-first sdired-optional-switches))
    (add-to-list 'sdired-optional-switches sdired-switch-for-directory-first))
  (sdired-refresh))

(defun sdired-reset ()
  "Reset sort configurations"
  (interactive)
  (setq sdired-base-switches sdired-switches-for-name
        sdired-other-base-switches ""
        sdired-optional-switches '())
  (sdired-refresh))

(defun sdired-set-mode-line ()
  "Set mode line display according to `sdired-base-switches'.
Mode line displays \"by name\", \"by date\", \"by size\",
or \"by type\". If switches are other than builtin,
switches are shown literally."
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
and `sdired-optional-switches'.
Subsequently, call `sdired-set-mode-line'"
  (dired-sort-other
   (mapconcat 'identity
              (cons sdired-base-switches sdired-optional-switches)
              " "))
  (sdired-set-mode-line))


(provide 'sdired)

;;; sdired.el ends here
