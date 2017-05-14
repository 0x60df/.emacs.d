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
        (lisp-interaction-mode)
        (run-hook-with-args 'scratchb-after-revert-hook))))

;;;###autoload
(defun scratchb-snapshot ()
  "Write *scratch* buffer content to `scratchb-snapshot-directory'"
  (interactive)
  (with-current-buffer "*scratch*"
    (save-restriction
      (widen)
      (write-region (point-min) (point-max)
                    (concat scratchb-snapshot-directory
                            "/"
                            (format-time-string "%Y%m%d%H%M%S.el"
                                                (current-time)))))))

(provide 'scratchb)

;;; scratchb.el ends here
