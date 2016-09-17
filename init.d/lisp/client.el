
;;;; client.el


(defun server-after-first-make-frame-function (frame)
    (select-frame frame)
    (remove-hook 'after-make-frame-functions
                 'server-after-first-make-frame-function))

(defmacro server-eval-after-first-make-frame (name &rest body)
  `(cond ((daemonp)
          (defadvice server-after-first-make-frame-function (after ,name)
            ,@body))
         (t ,@body)))

(when (daemonp)
  (add-hook 'after-init-hook
            (lambda () (ad-activate 'server-after-first-make-frame-function)))
  (add-hook 'after-make-frame-functions
            'server-after-first-make-frame-function))

(defun server-other-client-frame (arg)
  (interactive "p")
  (let* ((initial-frame (selected-frame))
         (frame initial-frame))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (while (or (not (eq (frame-visible-p frame) t))
                 (eq (frame-parameter frame 'client)
                     (frame-parameter initial-frame 'client)))
        (setq frame (next-frame frame)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (while (or (not (eq (frame-visible-p frame) t))
                 (eq (frame-parameter frame 'client)
                     (frame-parameter initial-frame 'client)))
        (setq frame (previous-frame frame)))
      (setq arg (1+ arg)))
    (select-frame-set-input-focus frame)))

(defun server-other-frame-on-selected-client (arg)
  (interactive "p")
  (let* ((initial-frame (selected-frame))
         (frame initial-frame))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (while (or (not (eq (frame-visible-p frame) t))
                 (not (eq (frame-parameter frame 'client)
                          (frame-parameter initial-frame 'client))))
        (setq frame (next-frame frame)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (while (or (not (eq (frame-visible-p frame) t))
                 (not (eq (frame-parameter frame 'client)
                          (frame-parameter initial-frame 'client))))
        (setq frame (previous-frame frame)))
      (setq arg (1+ arg)))
    (select-frame-set-input-focus frame)))

(eval-after-load 'server
  '(progn
     (global-set-key (kbd "C-.") 'server-other-frame-on-selected-client)
     (global-set-key (kbd "C-M-.")
                     (lambda (arg)
                       (interactive "p")
                       (server-other-frame-on-selected-client (* arg -1))))
     (global-set-key (kbd "s-.") 'server-other-client-frame)
     (global-set-key (kbd "s-M-.")
                     (lambda (arg)
                       (interactive "p")
                       (server-other-client-frame (* arg -1))))))
