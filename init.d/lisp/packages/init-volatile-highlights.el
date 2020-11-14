
;;;; init-volatile-highlights.el


(premise init)
(premise inst-volatile-highlights)

(with-eval-after-load 'volatile-highlights
  (setcdr (assq 'volatile-highlights-mode minor-mode-alist) '("")))

(defvar vhl/clean-active-mark-flag t
  "Flag to determine wheather do `vhl/clean-active-mark' or not.")

(defun vhl/clean-active-mark ()
  "`deactivate-mark' once and revert setup."
  (unwind-protect
      (if vhl/clean-active-mark-flag (deactivate-mark))
    (remove-hook 'post-command-hook #'vhl/clean-active-mark)
    (advice-remove 'activate-mark #'vhl/cancel-clean-active-mark)
    (setq vhl/clean-active-mark-flag t)))

(defun vhl/cancel-clean-active-mark (&rest args)
  "Cancel `vhl/clean-active-mark-flag' by setting flag."
  (setq vhl/clean-active-mark-flag nil))

(defun vhl/revert-transient-mark-mode (&rest args)
  "Turn on `transient-mark-mode' once and continue setup."
  (remove-hook 'pre-command-hook #'vhl/revert-transient-mark-mode)
  (transient-mark-mode)
  (add-hook 'post-command-hook #'vhl/clean-active-mark)
  (advice-add 'activate-mark :after #'vhl/cancel-clean-active-mark))

(defun vhl/volatile-active-mark (beg end)
  "Advising function for `vhl/add-range'.
Activate region between BEG and END if point is BEG or END,
and prepare clean setup."
  (cond ((eql beg (point))
         (transient-mark-mode 0)
         (push-mark end nil t))
        ((eql end (point))
         (transient-mark-mode 0)
         (push-mark beg nil t)))
  (add-hook 'pre-command-hook #'vhl/revert-transient-mark-mode))

(advice-add 'vhl/add-range :after #'vhl/volatile-active-mark)

(add-hook 'emacs-startup-hook #'volatile-highlights-mode)


(resolve init-volatile-highlights)
