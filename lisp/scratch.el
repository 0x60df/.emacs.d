;;; scratch.el --- generate new buffer instantly

;;; code:

(require 'shifter)

(defgroup scratch nil
  "Generate new buffer instantly"
  :group 'emacs)

(defvar scratch-list '() "Scratch buffer list which are not labeled.")

;;;###autoload
(defvar scratch-mode-map (make-sparse-keymap) "Keymap for scratch-mode.")

(defun scratch-shred ()
  "Kill current buffer"
  (interactive)
  (setq scratch-list (remove (current-buffer) scratch-list))
  (kill-buffer (current-buffer)))

(defun scratch-shred-all ()
  "Kill all scratch buffers which are not labeled."
  (interactive)
  (when(yes-or-no-p "Kill all scratch buffers? ")
      (mapc #'kill-buffer scratch-list)
      (setq scratch-list '())))

(defun scratch-label ()
  "Rename scratch buffer and remove it from `scratch-list'."
  (interactive)
  (when scratch-mode
    (rename-buffer (read-string "Lable: "))
    (setq scratch-list (remove (current-buffer) scratch-list))
    (scratch-mode -1)))

;;;###autoload
(define-minor-mode scratch-mode
  "Toggle `scratch-mode'."
  :group 'scratch
  :keymap 'scratch-mode-map)

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
      (add-to-list 'scratch-list buffer))))

(provide 'scratch)

;;; scratch.el ends here
