;;; scratchb.el --- *scratch* buffer utilities

;;; Commentary:

;;; code:

(defgroup scratchb nil
  "*scratch* buffer utilities"
  :group 'emacs)

(defcustom scratchb-snapshot-directory (concat user-emacs-directory "scratchb/")
  "Directory in which snapshots of scratch buffer are saved."
  :type 'directory
  :group 'scratchb)

(defcustom scratchb-default-directory "~/"
  "Directory which is selected when scratch buffer is reverted."
  :type 'directory
  :group'scratchb)

(defcustom scratchb-snapshot-limit 256
  "Number of limit for scratchb snapshot files."
  :type 'integer
  :group 'scratchb)

(defvar scratchb-before-flush-hook nil "Hook run before `scratchb-flush'.")

(defvar scratchb-after-flush-hook nil "Hook run after `scratchb-flush'.")

(defvar scratchb-before-revert-hook nil "Hook run before `scratchb-revert'.")

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
  (unless (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (cd scratchb-default-directory)
      (funcall initial-major-mode)
      (run-hook-with-args 'scratchb-after-revert-hook))))

;;;###autoload
(defun scratchb-snapshot ()
  "Write *scratch* buffer content to `scratchb-snapshot-directory'."
  (interactive)
  (when (file-writable-p scratchb-snapshot-directory)
    (with-current-buffer "*scratch*"
      (save-restriction
        (widen)
        (write-region (point-min) (point-max)
                      (concat scratchb-snapshot-directory
                              (format-time-string "%Y%m%d%H%M%S.el"
                                                  (current-time))))))
    (mapc (lambda (file)
            (if (and (stringp file)
                     (file-exists-p file))
                (delete-file file)))
          (nthcdr scratchb-snapshot-limit
                  (sort (file-expand-wildcards
                         (concat scratchb-snapshot-directory "*.el"))
                        #'string>)))))

;;;###autoload
(define-minor-mode scratchb-mode
  "Minor mode to hold utilities for *scratch* buffer."
  :group 'scratchb
  :keymap (make-sparse-keymap))

(defun scratchb--stick-after-change-major-mode ()
  "Enable `scratchb-sticky-mode' after major mode is changed.
This function is intended to be used only in
`scratchb-sticky-mode'."
  (add-hook 'after-change-major-mode-hook #'scratchb-sticky-mode))

(defun scratchb--stick-after-create-new-buffer ()
  "Enable `scratchb-sticky-mode' after new buffer is created.
This function is intended to be used only in
`scratchb-sticky-mode'."
  (let ((buffer (get-buffer "*scratch*")))
    (if buffer
        (with-current-buffer buffer
          (scratchb-sticky-mode)))))

;;;###autoload
(define-minor-mode scratchb-sticky-mode
  "Minor mode to keep `scratchb-mode' on, even with major mode change."
  :group 'scratchb
  :global t
  (if scratchb-sticky-mode
      (with-current-buffer "*scratch*"
        (remove-hook 'after-change-major-mode-hook #'scratchb-sticky-mode)
        (scratchb-mode)
        (add-hook 'change-major-mode-hook
                  #'scratchb--stick-after-change-major-mode nil t)
        (add-hook 'buffer-list-update-hook
                  #'scratchb--stick-after-create-new-buffer))
    (with-current-buffer "*scratch*"
      (scratchb-mode 0)
      (remove-hook 'change-major-mode-hook
                   #'scratchb--stick-after-change-major-mode t)
      (remove-hook 'buffer-list-update-hook
                   #'scratchb--stick-after-create-new-buffer))))

;;;###autoload
(define-minor-mode scratchb-auto-revert-mode
  "Minor mode to revert *scratch* buffer automatically."
  :group 'scratchb
  :global t
  (if scratchb-auto-revert-mode
      (progn
        (scratchb-revert)
        (add-hook 'buffer-list-update-hook #'scratchb-revert))
    (remove-hook 'buffer-list-update-hook #'scratchb-revert)))

;;;###autoload
(define-minor-mode scratchb-auto-snapshot-mode
  "Minor mode to take snapshot of *scratch* buffer automatically."
  :group 'scratchb
  :global t
  (if scratchb-auto-snapshot-mode
      (progn
        (add-hook 'scratchb-before-flush-hook #'scratchb-snapshot)
        (add-hook 'kill-emacs-hook #'scratchb-snapshot)
        (with-current-buffer "*scratch*"
          (add-hook 'kill-buffer-hook #'scratchb-snapshot nil t)))
    (remove-hook 'scratchb-before-flush-hook #'scratchb-snapshot)
    (remove-hook 'kill-emacs-hook #'scratchb-snapshot)
    (with-current-buffer "*scratch*"
      (remove-hook 'kill-buffer-hook #'scratchb-snapshot t))))

(provide 'scratchb)

;;; scratchb.el ends here
