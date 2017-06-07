;;; scratchb.el --- *scratch* buffer utilities

;;; code:

(defgroup scratchb nil
  "*scratch* buffer utilities"
  :group 'emacs)

;;;###autoload
(defcustom scratchb-snapshot-directory (concat user-emacs-directory "scratchb")
  "Directory in which snapshots of scratch buffer is saved."
  :group 'scratchb)

;;;###autoload
(defcustom scratchb-default-directory "~"
  "Directory which is selected when scratch buffer is reverted."
  :group'scratchb)

;;;###autoload
(defcustom scratchb-snapshot-limit 256
  "Number of limit for scratchb snapshot files."
  :group 'scratchb)

;;;###autoload
(defvar scratchb-before-flush-hook nil "Hook run before `scratchb-flush'.")

;;;###autoload
(defvar scratchb-after-flush-hook nil "Hook run after `scratchb-flush'.")

;;;###autoload
(defvar scratchb-before-revert-hook nil "Hook run before `scratchb-revert'.")

;;;###autoload
(defvar scratchb-after-revert-hook nil "Hook run after `scratchb-revert'.")

;;;###autoload
(defun scratchb-flush ()
  "`erase-buffer' and `set-buffer-modified-p' nil on *scratch* buffer."
  (interactive)
  (run-hook-with-args 'scratchb-before-flush-hook)
  (with-current-buffer "*scratch*"
    (erase-buffer)
    (set-buffer-modified-p nil)
    (run-hook-with-args 'scratchb-after-flush-hook)))

;;;###autoload
(defun scratchb-revert ()
  "Generate *scratch* buffer if it does not exist."
  (interactive)
  (run-hook-with-args 'scratchb-before-revert-hook)
  (if (not (member (get-buffer "*scratch*") (buffer-list)))
      (with-current-buffer (get-buffer-create "*scratch*")
        (cd scratchb-default-directory)
        (lisp-interaction-mode)
        (run-hook-with-args 'scratchb-after-revert-hook))))

;;;###autoload
(defun scratchb-snapshot ()
  "Write *scratch* buffer content to `scratchb-snapshot-directory'"
  (interactive)
  (when (file-writable-p scratchb-snapshot-directory)
    (with-current-buffer "*scratch*"
      (save-restriction
        (widen)
        (write-region (point-min) (point-max)
                      (concat scratchb-snapshot-directory
                              "/"
                              (format-time-string "%Y%m%d%H%M%S.el"
                                                  (current-time))))))
    (mapc #'delete-file (nthcdr scratchb-snapshot-limit
                                (reverse (sort (file-expand-wildcards
                                                "~/.emacs.d/scratchb/*.el")
                                               #'string<))))))

(defun scratchb--snapshot-when-scratchb ()
  "Snapshot scratchb when current-frame is *scratch* buffer."
  (if (equal "*scratch*" (buffer-name)) (scratchb-snapshot)))

;;;###autoload
(define-minor-mode scratchb-mode
  "Toggle `scratchb-mode'.

In `scratchb-mode' *scratch*
  - buffer is reverted automatically
  - snapshot of content is taken when quit emacs or flush *scratch* buffer"
  :group 'scratchb
  :global t
  (if scratchb-mode
      (progn
        (scratchb-revert)
        (add-hook 'scratchb-before-flush-hook #'scratchb-snapshot)
        (add-hook 'kill-emacs-hook #'scratchb-snapshot)
        (add-hook 'kill-buffer-hook #'scratchb--snapshot-when-scratchb)
        (add-hook 'buffer-list-update-hook #'scratchb-revert))
    (remove-hook 'scratchb-before-flush-hook #'scratchb-snapshot)
    (remove-hook 'kill-emacs-hook #'scratchb-snapshot)
    (remove-hook 'kill-buffer-hook #'scratchb--snapshot-when-scratchb)
    (remove-hook 'buffer-list-update-hook #'scratchb-revert)))

(provide 'scratchb)

;;; scratchb.el ends here
