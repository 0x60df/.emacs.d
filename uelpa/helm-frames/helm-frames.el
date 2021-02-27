;;; helm-frames.el --- helm interface for frames

;; Copyright (C) 2020 0x60DF

;; Package-Requires: (helm)

;;; Commentary:

;; Helm interface for selecting frames.

;;; code:

(require 'helm)
(require 'helm-source)
(require 'helm-buffers)

(defvar helm-frames--raised-frame-list nil
  "Transiently stores frames which are raised by transient action.")

(defclass helm-source-frames (helm-source-sync)
  ((candidates :initform
               (lambda () (mapcar (lambda (frame)
                                    (cons (format "%s" frame) frame))
                                  (frame-list))))
   (action :initform
           '(("Focus frame" . select-frame-set-input-focus)
             ("Raise frame(s)" . (lambda (dummy)
                                   (mapc #'raise-frame
                                         (helm-marked-candidates))))
             ("Delete frame(s)" . (lambda (dummy)
                                    (mapc #'delete-frame
                                          (helm-marked-candidates))))))
   (persistent-action
    :initform
    (lambda (candidate)
      (cond ((eq candidate (selected-frame))
             (raise-frame candidate))
            ((memq candidate helm-frames--raised-frame-list)
             (lower-frame candidate)
             (setq helm-frames--raised-frame-list
                   (delq candidate helm-frames--raised-frame-list)))
            (t (raise-frame candidate)
               (setq helm-frames--raised-frame-list
                     (cons candidate helm-frames--raised-frame-list))))))
   (persistent-help
    :initform "Raise or Lower frame")
   (cleanup :initform (lambda () (setq helm-frames--raised-frame-list nil)))
   (match-part :initform (lambda (candidate)
                           (replace-regexp-in-string
                            "^#<frame \\(.*\\) .*>$"
                            "\\1"
                            (format "%s" candidate))))))

(defvar helm-source-all-frames (helm-make-source "Frames" 'helm-source-frames)
  "Helm source for frames.")

(defvar helm-source-frames-on-selected-client
  (helm-make-source "Frames" 'helm-source-frames
    :candidate-transformer
    (lambda (candidates)
      (let ((proc (frame-parameter nil 'client)))
        (if proc
            (seq-filter
             (lambda (f) (eq proc (frame-parameter (cdr f) 'client)))
             candidates)))))
  "Helm source for frames on selected client.")

(defvar helm-source-typical-frames-on-each-client
  (helm-make-source "Typical frames on client" 'helm-source-frames
    :candidate-transformer
    (lambda (candidates)
      (let (proc-list
            transformed-candidates)
        (mapc (lambda (name-frame)
                (let ((proc (frame-parameter (cdr name-frame) 'client)))
                  (when (and proc (not (memq proc proc-list)))
                    (push proc proc-list)
                    (push name-frame transformed-candidates))))
              candidates)
        transformed-candidates)))
  "Helm source for typical frames on each client.")

(defvar helm-source-buffers-for-frame
  (helm-build-sync-source "Buffers"
    :candidates #'helm-buffer-list
    :action '(("Make frame and switch to buffer" .
               (lambda (candidate)
                 (select-frame-set-input-focus (make-frame))
                 (switch-to-buffer candidate))))
    :persistent-action #'helm-buffers-list-persistent-action
    :persistent-help "Show this buffer"
    :candidate-transformer (lambda (candidates)
                             (helm-skip-boring-buffers
                              candidates
                              helm-source-buffers-for-frame)))
  "Helm source for buffers which will be selected in new frame.")

;;;###autoload
(defun helm-frames ()
  "Helm command for frames."
  (interactive)
  (helm :sources '(helm-source-all-frames
                   helm-source-buffers-for-frame)
        :buffer "*helm frame*"))

;;;###autoload
(defun helm-frames-on-selected-client ()
  "Helm command for frames on selected client."
  (interactive)
  (helm :sources '(helm-source-frames-on-selected-client
                   helm-source-buffers-for-frame)
        :buffer "*helm frame*"))

;;;###autoload
(defun helm-typical-frames-on-each-client ()
  "Helm command for typical frames on each client."
  (interactive)
  (helm :sources '(helm-source-typical-frames-on-each-client
                   helm-source-buffers-for-frame)
        :buffer "*helm frame*"))

(provide 'helm-frames)

;;; helm-frames.el ends here
