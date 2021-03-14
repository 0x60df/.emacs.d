;;; scratch.el --- generate new buffer instantly

;;; Commentary:

;;; Code:

(require 'shifter)

(defgroup scratch nil
  "Generate new buffer instantly"
  :group 'emacs)

(defcustom scratch-hook nil
  "Normal hook run after `scratch' buffer is generated."
  :type 'hook
  :group 'scratch)

(defcustom scratch-before-shred-hook nil
  "Normal hook run before `scratch' buffer is shredded."
  :type 'hook
  :group 'scratch)

(defcustom scratch-before-label-hook nil
  "Normal hook run before `scratch' buffer is labeled."
  :type 'hook
  :group 'scratch)

(defcustom scratch-after-write-hook nil
  "Normal hook run after `scratch' buffer is written ."
  :type 'hook
  :group 'scratch)

(defcustom scratch-after-write-labeled-hook nil
  "Normal hook run after labeled `scratch' buffer is written."
  :type 'hook
  :group 'scratch)

(defcustom scratch-snapshot-directory (concat user-emacs-directory "scratch/")
  "Directory path for snapshots of `scratch' buffer."
  :type 'directory
  :group 'scratch)

(defcustom scratch-snapshot-limit 256
  "Limit number for snapshots of `scratch' buffer."
  :type 'integer
  :group 'scratch)

(defcustom scratch-auto-snapshot-delay 2.0
  "Delay time by second for auto snapshot of `scratch' buffer."
  :type 'number
  :group 'scratch)

(defvar scratch-auto-snapshot-timer nil
  "Timer for snapshot of `scratch' buffer.")
(make-variable-buffer-local 'scratch-auto-snapshot-timer)

(defvar scratch-preserved-mode nil
  "Preserved mode symbol for `scratch-preserving-mode'.")
(put 'scratch-preserved-mode 'permanent-local t)
(make-variable-buffer-local 'scratch-preserved-mode)

(defvar scratch-list '() "List of `scratch' buffer which is not labeled.")

(defvar scratch-labeled-list '() "List of labeled `scratch' buffer.")

(defun scratch--delete-from-list (&optional scratch)
  "Delete SCRATCH from `scratch-list'.
If SCRATCH is nil, delete current `scratch' buffer."
  (setq scratch-list (delq (or scratch (current-buffer)) scratch-list)))

(defun scratch--delete-from-labeled-list (&optional scratch)
  "Delete labled SCRATCH from `scratch-labeled-list'.
If SCRATCH is nil, delete current `scratch' buffer."
  (setq scratch-list (delq (or scratch (current-buffer)) scratch-list)))

(defun scratch--write (&optional scratch)
  "Writing function for SCRATCH buffer.
This function is intended to work with
`write-contents-functions'
Therefore, return t if succeeded.
If SCRATCH is nil, write current `scratch' buffer."
  (let ((s (car (memq (or scratch (current-buffer)) scratch-list))))
    (if s
        (with-current-buffer s
          (let ((wcf write-contents-functions)
                (sawh scratch-after-write-hook))
            (unwind-protect
                (progn
                  (setq write-contents-functions nil)
                  (remove-hook 'change-major-mode-hook
                               #'scratch--stick-write-function t)
                  (save-buffer)
                  (setq scratch-list (delq (current-buffer) scratch-list))
                  (remove-hook 'kill-buffer-hook #'scratch--delete-from-list t)
                  (run-hooks 'sawh)
                  (setq wcf nil)
                  t)
              (when wcf
                (setq write-contents-functions wcf)
                (add-hook 'change-major-mode-hook
                          #'scratch--stick-write-function nil t))))))))

(defun scratch--write-labeled (&optional scratch)
  "Writing function for labled SCRATCH buffer.
This function is intended to work with
`write-contents-functions'
Therefore, return t if succeeded.
If SCRATCH is nil, write current `scratch' buffer."
  (let ((s (car (memq (or scratch (current-buffer)) scratch-labeled-list))))
    (if s
        (with-current-buffer s
          (let ((directory (read-directory-name "Directory to save in: ")))
            (if (not (file-writable-p directory))
                (user-error "Specified directory is not writable")
              (let ((wcf write-contents-functions)
                    (sawlh scratch-after-write-labeled-hook))
                (unwind-protect
                    (progn
                      (setq write-contents-functions nil)
                      (remove-hook 'change-major-mode-hook
                                   #'scratch--stick-write-labeled-function t)
                      (write-file (concat directory (buffer-name)))
                      (setq scratch-labeled-list
                            (delq (current-buffer) scratch-labeled-list))
                      (remove-hook 'kill-buffer-hook
                                   #'scratch--delete-from-labeled-list t)
                      (run-hooks 'sawlh)
                      (setq wcf nil)
                      t)
                  (when wcf
                    (setq write-contents-functions wcf)
                    (add-hook 'change-major-mode-hook
                              #'scratch--stick-write-labeled-function
                              nil t))))))))))

(defun scratch--stick-write-function ()
  "Re set `scratch--write' function after major mode change."
  (add-hook 'after-change-major-mode-hook #'scratch--set-sticky-write-function))

(defun scratch--set-sticky-write-function ()
  "Set `scratch--write' even with major mode change."
  (remove-hook 'after-change-major-mode-hook
               #'scratch--set-sticky-write-function)
  (with-local-quit
    (if (or (null write-contents-functions)
            (yes-or-no-p "Override write-contents-functions?: "))
        (setq write-contents-functions
              `(scratch--write ,@write-contents-functions))))
  (add-hook 'change-major-mode-hook #'scratch--stick-write-function nil t))

(defun scratch--stick-write-labeled-function ()
  "Re set `scratch--write-labeled' function after major mode change."
  (add-hook 'after-change-major-mode-hook
            #'scratch--set-sticky-write-labeled-function))

(defun scratch--set-sticky-write-labeled-function ()
  "Set `scratch--write-labeled' even with major mode change."
  (remove-hook 'after-change-major-mode-hook
               #'scratch--set-sticky-write-labeled-function)
  (with-local-quit
    (if (or (null write-contents-functions)
            (yes-or-no-p "Override write-contents-functions?: "))
        (setq write-contents-functions
              `(scratch--write-labeled ,@write-contents-functions))))
  (add-hook 'change-major-mode-hook
            #'scratch--stick-write-labeled-function nil t))

(defun scratch-shred (&optional scratch)
  "Kill SCRATCH buffer.
If SCRATCH is nil, kill current `scratch' buffer."
  (interactive)
  (let ((s (car (memq (or scratch (current-buffer)) scratch-list))))
    (if s
        (with-current-buffer s
          (run-hooks 'scratch-before-shred-hook)
          (setq scratch-list (delq s scratch-list))
          (kill-buffer s)))))

(defun scratch-shred-all ()
  "Kill all `scratch' buffers which are not labeled."
  (interactive)
  (if (y-or-n-p "Kill all scratch buffers? ")
    (mapc #'scratch-shred scratch-list)))

(defun scratch-label (&optional scratch)
  "Rename SCRATCH buffer and remove it from `scratch-list'.
If SCRATCH is nil, lable current `scratch' buffer."
  (interactive)
  (let ((s (car (memq (or scratch (current-buffer)) scratch-list))))
    (if s
        (with-current-buffer s
          (run-hooks 'scratch-before-label-hook)

          (rename-buffer (read-string "Lable: "))

          (setq scratch-list (delq s scratch-list))
          (add-to-list 'scratch-labeled-list s)
          (remove-hook 'kill-buffer-hook #'scratch--delete-from-list t)
          (add-hook 'kill-buffer-hook
                    #'scratch--delete-from-labeled-list nil t)

          (setq write-contents-functions
                (delq #'scratch--write write-contents-functions))
          (remove-hook 'change-major-mode-hook
                       #'scratch--stick-write-function t)
          (scratch--set-sticky-write-labeled-function)))))

(defun scratch-snapshot (&optional scratch)
  "Snapshot SCRATCH buffer.
If SCRATCH is nil, snapshot current `scratch' buffer."
  (interactive)
  (unless (file-exists-p scratch-snapshot-directory)
    (make-directory scratch-snapshot-directory t))
  (when (file-writable-p scratch-snapshot-directory)
    (let ((s (car (memq (or scratch (current-buffer))
                        `(,@scratch-list ,@scratch-labeled-list)))))
      (when s
        (with-current-buffer s
          (save-restriction
            (widen)
            (write-region (point-min) (point-max)
                          (concat scratch-snapshot-directory
                                  (replace-regexp-in-string
                                   "\\*\\(.+\\)\\*" "\\1"
                                   (buffer-name)))
                          nil 'quiet)))
        (mapc (lambda (file)
                (if (and (stringp file)
                         (file-exists-p file))
                    (delete-file file)))
              (nthcdr scratch-snapshot-limit
                      (sort (file-expand-wildcards
                             (concat scratch-snapshot-directory "*"))
                            #'file-newer-than-file-p)))))))

(defun scratch--stick-after-change-major-mode ()
  "Enable `scratch-sticking-mode' after major mode is changed.
This function is intended to be used only in
`scratch-sticking-mode'."
  (add-hook 'after-change-major-mode-hook #'scratch-sticking-mode))

(defun scratch--try-quit-sticky ()
  "Try quit stick on `before-save-hook'.
Disable `scratch-sticking-mode' and setup follow up
function `scratch--revert-sticky' on
`post-command-hook' locally, which revert sticky mode.
If file is saved successfully,`scratch--revert-sticky' will
be discarded because local variables including
`post-command-hook' will be killed."
  (unwind-protect
      (scratch-sticking-mode 0)
    (add-hook 'post-command-hook #'scratch--revert-sticky nil t)))

(defun scratch--revert-sticky ()
  "Revert stick according to the state of visit.
This function is intended to work with `post-command-hook'."
  (unwind-protect
      (scratch-sticking-mode)
    (remove-hook 'post-command-hook #'scratch--revert-sticky t)))

(defun scratch--setup-auto-snapshot-timer (begin end range)
  "Setup timer for snapshot of `scratch' buffer.
This function is intended to hooked to
`after-change-functions'.
Therefore, this function receives three arguments BEGIN,
END and RANGE.  However, all of them are ignored."
  (unless scratch-auto-snapshot-timer
    (setq scratch-auto-snapshot-timer
          (run-with-idle-timer
           scratch-auto-snapshot-delay
           nil
           `(lambda ()
              (if (buffer-live-p ,(current-buffer))
                  (with-current-buffer ,(current-buffer)
                    (scratch-snapshot)
                    (setq scratch-auto-snapshot-timer nil))))))))

(defun scratch--restore-mode ()
  "Restore `scratch-preserved-mode'."
  (when (and buffer-file-name
             (functionp scratch-preserved-mode))
    (funcall scratch-preserved-mode)
    (kill-local-variable 'scratch-preserved-mode)))

(defun scratch--preserve-mode ()
  "Preserve major mode for `scratch-preserving-mode'."
  (setq scratch-preserved-mode major-mode))

;;;###autoload
(define-minor-mode scratch-mode
  "Minor mode to enable features for `scratch' buffer."
  :group 'scratch
  :keymap (make-sparse-keymap))

(define-minor-mode scratch-sticking-mode
  "Minor mode to keep local preferences even with major mode change.
Any functions which should be called on major mode change
can be added to `scratch-sticking-mode-hook'.
Typically, the following forms keep
`scratch-auto-snapshot-mode' active after major mode change.
(add-hook 'scratch-sticking-mode-hook
          (lambda ()
            (if scratch-sticking-mode
                (scratch-auto-snapshot-mode))))"
  :group 'scratch
  (if scratch-sticking-mode
      (progn
        (remove-hook 'after-change-major-mode-hook #'scratch-sticking-mode)
        (add-hook 'before-save-hook #'scratch--try-quit-sticky nil t)
        (add-hook 'change-major-mode-hook
                  #'scratch--stick-after-change-major-mode nil t))
    (remove-hook 'before-save-hook #'scratch--try-quit-sticky t)
    (remove-hook 'change-major-mode-hook
                 #'scratch--stick-after-change-major-mode t)))

(define-minor-mode scratch-auto-snapshot-mode
  "Minor mode to take snapshot of `scratch' buffer automatically."
  :group 'scratch
  (if scratch-auto-snapshot-mode
      (progn
        (add-hook 'after-change-functions
                  #'scratch--setup-auto-snapshot-timer nil t)
        (add-hook 'scratch-before-shred-hook #'scratch-snapshot nil t)
        (add-hook 'scratch-before-label-hook #'scratch-snapshot nil t))
    (remove-hook 'after-change-functions
                 #'scratch--setup-auto-snapshot-timer t)
    (remove-hook 'scratch-before-shred-hook #'scratch-snapshot t)
    (remove-hook 'scratch-before-label-hook #'scratch-snapshot t)))

(define-minor-mode scratch-preserving-mode
  "Minor mode to preserve major mode after `scratch' buffer is saved."
  :group 'scratch
  (if scratch-preserving-mode
      (progn
        (add-hook 'before-save-hook #'scratch--preserve-mode nil t)
        (add-hook 'scratch-after-write-hook #'scratch--restore-mode nil t)
        (add-hook 'scratch-after-write-labeled-hook
                  #'scratch--restore-mode nil t))
    (remove-hook 'before-save-hook #'scratch--preserve-mode t)
    (remove-hook 'scratch-after-write-hook #'scratch--restore-mode t)
    (remove-hook 'scratch-after-write-labeled-hook #'scratch--restore-mode t)))

;;;###autoload
(defun scratch ()
  "Generate new buffer instantly."
  (interactive)
  (let ((inhibit-quit t)
        (buffer (generate-new-buffer
                 (concat "*" (substring (format "%07x" (random)) -7) "*"))))
    (switch-to-buffer buffer)
    (let (success)
      (unwind-protect
          (setq success (with-local-quit (shifter-shift-major-mode) t))
        (if (not success)
            (kill-buffer buffer)
          (add-to-list 'scratch-list buffer)
          (add-hook 'kill-buffer-hook #'scratch--delete-from-list nil t)
          (scratch--set-sticky-write-function)
          (run-hooks 'scratch-hook))))))

(provide 'scratch)

;;; scratch.el ends here
