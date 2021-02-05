
;;;; init-volatile-highlights.el


(premise init)
(premise subr)
(premise mode-line)
(premise inst-volatile-highlights)

(with-eval-after-load 'volatile-highlights
  (modify-minor-mode-lighter 'volatile-highlights-mode ""))

(defcustom vhl/commands-requring-volatilizing-active-mark
  '(undo)
  "List of commands who require volatilizing active mark.
Mmark will be volatilized before the commands are callded."
  :type '(repeat function)
  :group 'user)

(defvar vhl/volatilize-active-mark-flag t
  "Flag to determine wheather volatilize volatile active mark.")

(defun vhl/cancel-volatilize-active-mark (&rest args)
  "Cancel volatilize volatile active mark."
  (setq vhl/volatilize-active-mark-flag nil))

(defun vhl/volatile-active-mark (beg end)
  "Advising function for `vhl/add-range'.
Activate region between BEG and END if point is BEG or END.
Activated region will be volarized after next command,
unless the command is `activate-mark'."
  (cond ((eql beg (point))
         (transient-mark-mode 0)
         (push-mark end nil t))
        ((eql end (point))
         (transient-mark-mode 0)
         (push-mark beg nil t)))
  (add-hook-for-once
   'pre-command-hook
   (lambda ()
     (transient-mark-mode)
     (when (memq this-command vhl/commands-requring-volatilizing-active-mark)
       (deactivate-mark))
     (add-hook-for-once
      'post-command-hook
      (lambda ()
        (unwind-protect
            (if vhl/volatilize-active-mark-flag (deactivate-mark))
          (advice-remove 'activate-mark #'vhl/cancel-volatilize-active-mark)
          (setq vhl/volatilize-active-mark-flag t))))
     (advice-add 'activate-mark :after #'vhl/cancel-volatilize-active-mark))))

(with-eval-after-load 'volatile-highlights
  (advice-add 'vhl/add-range :after #'vhl/volatile-active-mark))

(add-hook 'emacs-startup-hook #'volatile-highlights-mode)


(resolve init-volatile-highlights)
