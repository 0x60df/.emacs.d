
;;;; frame.el


(defvar after-make-terminal-functions nil)
(defadvice make-frame (around list-terminal)
  (let ((existing-terminal-list (terminal-list)))
    ad-do-it
    (let ((made-frame-terminal (frame-terminal ad-return-value)))
      (unless (memq made-frame-terminal existing-terminal-list)
        (run-hook-with-args
         'after-make-terminal-functions made-frame-terminal)))))
(ad-activate 'make-frame)

(defun increase-frame-alpha (&optional arg frame)
  (interactive "p")
  (set-frame-parameter
   frame
   'alpha
   (if (null (frame-parameter frame 'alpha))
       100
     (let ((result (+ (frame-parameter frame 'alpha) arg)))
       (if (and (<= 0 result)(<= result 100))
           result
         (frame-parameter frame 'alpha))))))

(defun decrease-frame-alpha (&optional arg frame)
  (interactive "p")
  (set-frame-parameter
   frame
   'alpha
   (if (null (frame-parameter frame 'alpha))
       100
     (let ((result (- (frame-parameter frame 'alpha) arg)))
       (if (and (<= 0 result)(<= result 100))
           result
         (frame-parameter frame 'alpha))))))

(defun set-frame-alpha (arg &optional frame)
  (interactive "Nalpha: ")
  (set-frame-parameter frame 'alpha arg))

(defun toggle-frame-opacity (&optional frame)
  (interactive)
  (if (or (null (frame-parameter frame 'alpha))
          (= (frame-parameter frame 'alpha) 100))
      (set-frame-parameter frame 'alpha  0)
    (set-frame-parameter frame 'alpha 100)))

(defun increase-all-frames-alpha (&optional arg)
  (interactive "p")
  (mapc (lambda (frame) (increase-frame-alpha arg frame))
        (frame-list)))

(defun decrease-all-frames-alpha (&optional arg)
  (interactive "p")
  (mapc (lambda (frame) (decrease-frame-alpha arg frame))
        (frame-list)))

(defun set-all-frames-alpha (arg)
  (interactive "Nalpha: ")
  (modify-all-frames-parameters `((alpha . ,arg))))

(defun toggle-all-frames-opacity ()
  (interactive)
  (if (or (null (frame-parameter nil 'alpha))
          (= (frame-parameter nil 'alpha) 100))
      (modify-all-frames-parameters '((alpha . 0)))
    (modify-all-frames-parameters '((alpha . 100)))))
