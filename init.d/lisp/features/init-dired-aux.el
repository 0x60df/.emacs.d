
;;;; init-dired-aux.el


(premise init)

(eval-when-compile (require 'dired))

(declare-function dired-current-directory "dired")

(defun dired-select-window-silently ()
  "Ask user and `select-window' answered window."
  (let ((window (selected-window))
        (directories
         (mapcan (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (when (eq major-mode 'dired-mode)
                       (list (cons (dired-current-directory)
                                   w)))))
                 (delq (selected-window)
                       (window-list-1 nil 'nomini 'visible)))))
    (if (< 1 (length directories))
        (let ((target (completing-read "Target: " directories nil t nil nil
                                       (car directories))))
          (select-window (cdr (assoc target directories)))
          (select-window window)))))

(defun dired-dwim-copy (&optional arg)
  "`dired-do-copy' but with  enhanced dwim."
  (interactive "P")
  (dired-select-window-silently)
  (let ((dired-dwim-target 'dired-dwim-target-recent))
    (funcall-interactively 'dired-do-copy arg)))

(defun dired-dwim-rename (&optional arg)
  "`dired-do-rename' but with  enhanced dwim."
  (interactive "P")
  (dired-select-window-silently)
  (let ((dired-dwim-target 'dired-dwim-target-recent))
    (funcall-interactively 'dired-do-copy arg)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ", c") #'dired-dwim-copy)
  (define-key dired-mode-map (kbd ", r") #'dired-dwim-rename))


(resolve init-dired-aux)


