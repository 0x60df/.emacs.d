;;; scratch.el --- generate new buffer instantly

;;; code:

(require 'shifter)

(defgroup scratch nil
  "Generate new buffer instantly"
  :group 'emacs)

(defcustom scratch-snapshot-directory (concat user-emacs-directory "scratch/")
  "Directory path for snapshots."
  :type 'directory
  :group 'scratch)

(defcustom scratch-snapshot-limit 256
  "Limit number for snapshots."
  :type 'integer
  :group 'scratch)

(defcustom scratch-auto-snapshot t
  "Flag if snapshots for scratches are saved automatically."
  :type 'boolean
  :group 'scratch)

(defcustom scratch-auto-snapshot-delay 2.0
  "Delay time by second for auto snapshot."
  :type 'number
  :group 'scratch)

(defvar scratch-auto-snapshot-timer nil "Timer for snapshot.")
(make-variable-buffer-local 'scratch-auto-snapshot-timer)

(defvar scratch-list '() "Scratch buffer list which are not labeled.")

(defvar scratch-mode-map (make-sparse-keymap) "Keymap for scratch-mode.")

(defun scratch-snapshot ()
  "Snapshot scratch buffer."
  (interactive)
  (unless (file-exists-p scratch-snapshot-directory)
    (make-directory scratch-snapshot-directory t))
  (when (file-writable-p scratch-snapshot-directory)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max)
                    (concat scratch-snapshot-directory
                            "/"
                            (replace-regexp-in-string "\\*\\(.+\\)\\*" "\\1"
                                                      (buffer-name)))
                    nil 'quiet))
    (mapc (lambda (file)
            (if (and (stringp file)
                     (file-exists-p file))
                (delete-file file)))
          (nthcdr scratch-snapshot-limit
                  (sort (file-expand-wildcards
                         (concat scratch-snapshot-directory "*"))
                        #'file-newer-than-file-p)))))

(defun scratch--setup-auto-snapshot-timer (begin end range)
  "Setup timer for snapshot. This works with `after-change-functions'"
  (if scratch-auto-snapshot
      (unless scratch-auto-snapshot-timer
        (setq scratch-auto-snapshot-timer
              (run-with-idle-timer
               scratch-auto-snapshot-delay
               nil
               `(lambda ()
                  (with-current-buffer ,(current-buffer)
                    (scratch-snapshot)
                    (setq scratch-auto-snapshot-timer nil))))))))

(defun scratch--revert-or-quit-auto-snapshot ()
  "Revert or quit auto snapshot according to the state of visit.
If scratch does not visit file when this function is called,
revert auto snapshot. Otherwise, do quit auto snapshot.
This function is intended to be added to `pre-command-hook'."
  (unwind-protect
      (if buffer-file-name
          (setq write-contents-functions
                (delq #'scratch--try-quit-auto-snapshot
                      write-contents-functions))
        (add-hook 'after-change-functions
                  #'scratch--setup-auto-snapshot-timer nil t))
    (remove-hook 'pre-command-hook
                 #'scratch--revert-or-quit-auto-snapshot t)))

(defun scratch--try-quit-auto-snapshot ()
  "Try quit auto snapshot.
If scratch does not visit file until `pre-command-hook',
revert auto snapshot. Otherwise, do quit auto snapshot.
This function is intended to be added to
`write-contents-functions' as pre process.
Therefore, return nil if successfull."
  (remove-hook 'after-change-functions #'scratch--setup-auto-snapshot-timer t)
  (add-hook 'pre-command-hook
            #'scratch--revert-or-quit-auto-snapshot nil t)
  nil)

;;;###autoload
(define-minor-mode scratch-mode
  "Toggle `scratch-mode'."
  :group 'scratch
  :keymap 'scratch-mode-map
  (if scratch-mode
      (add-hook 'after-change-functions
                #'scratch--setup-auto-snapshot-timer nil t)
    (remove-hook 'after-change-functions
                 #'scratch--setup-auto-snapshot-timer t)))

(defun scratch-shred ()
  "Kill current buffer"
  (interactive)
  (if scratch-auto-snapshot (scratch-snapshot))
  (setq scratch-list (remove (current-buffer) scratch-list))
  (kill-buffer (current-buffer)))

(defun scratch-shred-all ()
  "Kill all scratch buffers which are not labeled."
  (interactive)
  (when (yes-or-no-p "Kill all scratch buffers? ")
    (mapc (lambda (scratch)
            (if scratch-auto-snapshot
                (with-current-buffer scratch
                  (scratch-snapshot)))
            (kill-buffer scratch))
          scratch-list)
      (setq scratch-list '())))

(defun scratch-label ()
  "Rename scratch buffer and remove it from `scratch-list'."
  (interactive)
  (when scratch-mode
    (rename-buffer (read-string "Lable: "))
    (if scratch-auto-snapshot (scratch-snapshot))
    (setq scratch-list (remove (current-buffer) scratch-list))
    (scratch-mode -1)
    (add-hook 'after-change-functions
              #'scratch--setup-auto-snapshot-timer nil t)
    (setq write-contents-functions
          (delq #'scratch--try-quit-auto-snapshot write-contents-functions))
    (if (or (null write-contents-functions)
            (yes-or-no-p "Override write-contents-functions: "))
        (setq write-contents-functions
              (cons (lambda ()
                      (let ((directory (read-directory-name
                                        "Directory to save in: ")))
                        (if (not (file-writable-p directory))
                            (user-error "Specified directory is not writable")
                          (let ((wcf write-contents-functions))
                            (unwind-protect
                                (progn
                                  (setq write-contents-functions nil)
                                  (write-file (concat directory (buffer-name)))
                                  (setq wcf nil)
                                  t)
                              (if wcf (setq write-contents-functions wcf)))))))
                    write-contents-functions)))
    (setq write-contents-functions
          (cons #'scratch--try-quit-auto-snapshot write-contents-functions))))

(defun scratch-mode-buffer-sticky ()
  "Enable `scratch-mode', and reserve enabling on change of major mode.
Reservation is restricted on current buffer."
  (scratch-mode 1)
  (add-hook 'change-major-mode-hook
            (lambda ()
              (add-hook
               'after-change-major-mode-hook #'scratch-mode-buffer-sticky))
            nil t)
  (remove-hook 'after-change-major-mode-hook #'scratch-mode-buffer-sticky))

;;;###autoload
(defun scratch ()
  "Generate new buffer instantly."
  (interactive)
  (let ((inhibit-quit t)
        (buffer (generate-new-buffer
                 (concat "*" (substring (format "%07x" (random)) -7) "*"))))
    (switch-to-buffer buffer)
    (if (not (with-local-quit (shifter-shift-major-mode) t))
        (kill-buffer buffer)
      (scratch-mode-buffer-sticky)
      (add-to-list 'scratch-list buffer)
      (setq write-contents-functions
          (cons #'scratch--try-quit-auto-snapshot write-contents-functions)))))

(provide 'scratch)

;;; scratch.el ends here
