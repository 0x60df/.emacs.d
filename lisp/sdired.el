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

(defface sdired-group '((t :inherit dired-header))
  "Face for group part of sdired."
  :group 'sdired)

(defface sdired-key '((t :inherit dired-mark))
  "Face for key part of sdired."
  :group 'sdired)

(defface sdired-active-key '((t :inherit dired-marked))
  "Face for active-key part of sdired."
  :group 'sdired)

(defface sdired-warning '((t :inherit dired-warning))
  "Face for warning part of sdired."
  :group 'sdired)


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
            (let* ((sequence
                    (read-key-sequence
                     (concat
                      (propertize "Key control       "
                                  'face '(sdired-group italic))
                      ": "
                      "["
                      (propertize "k" 'face '(sdired-key bold))
                      "] select key      ["
                      (propertize "t"  'face '(sdired-key bold))
                      "] toggle key\n"
                      (propertize "Custom switch     " 'face
                                  '(sdired-group italic))
                      ": "
                      "["
                      (propertize "e"  'face '(sdired-key bold))
                      "] edit switches   ["
                      (propertize "c"  'face '(sdired-key bold))
                      "] clean switches\n"
                      (propertize "Optional switch   " 'face
                                  '(sdired-group italic))
                      ": "
                      "["
                      (propertize
                       "r"  'face
                       (list (if (member sdired-switch-for-reverse
                                         sdired-optional-switches)
                                 'sdired-active-key
                               'sdired-key)
                             'bold))
                      "] reversal order  ["
                      (propertize
                       "d"  'face
                       (list (if (member sdired-switch-for-directory-first
                                         sdired-optional-switches)
                                 'sdired-active-key
                               'sdired-key)
                             'bold))
                      "] directory first\n"
                      (propertize "Sort control      "
                                  'face '(sdired-group italic))
                      ": "
                      "["
                      (propertize "s"  'face '(sdired-key bold))
                      "] reset           ["
                      (propertize "q"  'face '(sdired-key bold))
                      "] quit"
                      (if (stringp warning-message)
                          warning-message))))
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
                           (concat "\n"
                                   (propertize "Warning message   "
                                               'face '(sdired-group italic))
                                   ": "
                                   "["
                                   (propertize (key-description sequence)
                                               'face '(sdired-warning bold))
                                   "] "
                                   (format-message "`%s' is not supported"
                                                   command))))
                    (t (ignore-errors (call-interactively command))
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
