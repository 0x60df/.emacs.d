
;;;; init-volatile-highlights.el


(premise init)
(premise subr)
(premise inst-volatile-highlights)

(with-eval-after-load 'volatile-highlights
  (setcdr (assq 'volatile-highlights-mode minor-mode-alist) '("")))

(defvar vhl/volatize-active-mark-flag t
  "Flag to determine wheather volatize volatile active mark.")

(defun vhl/cancel-volatize-active-mark (&rest args)
  "Cancel volatize volatile active mark."
  (setq vhl/volatize-active-mark-flag nil))

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
     (add-hook-for-once
      'post-command-hook
      (lambda ()
        (unwind-protect
            (if vhl/volatize-active-mark-flag (deactivate-mark))
          (advice-remove 'activate-mark #'vhl/cancel-volatize-active-mark)
          (setq vhl/volatize-active-mark-flag t))))
     (advice-add 'activate-mark :after #'vhl/cancel-volatize-active-mark))))

(with-eval-after-load 'volatile-highlights
  (advice-add 'vhl/add-range :after #'vhl/volatile-active-mark))

(add-hook 'emacs-startup-hook #'volatile-highlights-mode)


(resolve init-volatile-highlights)
