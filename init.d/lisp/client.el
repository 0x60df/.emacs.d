
;;;; client.el



;;; base


;; eval after first make frame
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

;; functions
(defun server-client-frame-list ()
  (let* ((proc (frame-parameter nil 'client))
         (sift
          (if proc
              (lambda (fl)
                (letrec ((filter (lambda (p l)
                                   (cond ((null l) l)
                                         ((funcall p (car l))
                                          (funcall
                                           filter p (cdr l)))
                                         (t (cons
                                             (car l)
                                             (funcall
                                              filter
                                              p (cdr l))))))))
                  (funcall filter
                           (lambda (f)
                             (not (eq proc
                                      (frame-parameter
                                       f 'client))))
                           fl)))
            (symbol-function 'identity))))
    (funcall sift (frame-list))))

;; interactive functions
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

(defun server-increase-all-client-frames-alpha (&optional arg)
  (interactive "p")
  (mapc (lambda (frame) (increase-frame-alpha arg frame))
        (server-client-frame-list)))

(defun server-decrease-all-client-frames-alpha (&optional arg)
  (interactive "p")
  (mapc (lambda (frame) (decrease-frame-alpha arg frame))
        (server-client-frame-list)))

(defun server-set-all-client-frames-alpha (arg)
  (interactive "Nalpha: ")
  (mapc (lambda (frame) (set-frame-parameter frame 'alpha arg))
        (server-client-frame-list)))

(defun server-toggle-all-client-frames-opacity ()
  (interactive)
  (mapc 'toggle-frame-opacity (server-client-frame-list)))


;;; binding

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
                       (server-other-client-frame (* arg -1))))
     (global-set-key (kbd "C-s-+") 'server-toggle-all-client-frames-opacity)
     (global-set-key (kbd "C-H-+ s") 'server-set-all-client-frames-alpha)))
