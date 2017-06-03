;;; scratch.el --- generate new buffer instantly

;;; code:

(require 'shifter)

(defgroup scratch nil
  "Generate new buffer instantly"
  :group 'emacs)

;;;###autoload
(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'scratch-kill-current-buffer)
    map)
  "Keymap for scratch-mode.")

;;;###autoload
(defun scratch-kill-current-buffer ()
  "Kill current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(define-minor-mode scratch-mode
  "Toggle `scratch-mode'."
  :group 'scratch
  :keymap 'scratch-mode-map)

;;;###autoload
(defun scratch ()
  "Generate new buffer instantly."
  (interactive)
  (switch-to-buffer (generate-new-buffer
                     (concat "*" (substring (format "%07x" (random)) -7) "*")))
  (shifter-shift-major-mode)
  (scratch-mode 1))

(provide 'scratch)

;;; scratch.el ends here
