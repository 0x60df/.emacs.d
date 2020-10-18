
;;;; client.el


(premise init)
(premise frame)


;;; subr

(defun client-list ()
  "Return list of clients."
  (let (proc-list)
    (mapc (lambda (frame)
            (let ((proc (frame-parameter frame 'client)))
              (if (and proc (not (memq proc proc-list)))
                  (push proc proc-list))))
          (frame-list))
    proc-list))

(defun terminal-client-list (&optional terminal)
  "Return list of clients which belongs to TERMINAL."
  (let (proc-list)
    (mapc (lambda (frame)
            (let ((proc (frame-parameter frame 'client)))
              (if (and proc
                       (not (memq proc proc-list))
                       (eq (or terminal (frame-terminal))
                           (frame-terminal frame)))
                  (push proc proc-list))))
          (frame-list))
    proc-list))

(defun client-frame-list (&optional client)
  "Return list of frames which belongs to the client of current frame.
If CLIENT is non-nil, use CLIENT instead of the client of
current frame.
If current frame is not a client, return nil."
  (let* ((proc (or client (frame-parameter nil 'client))))
    (if proc
        (letrec ((filter (lambda (p l)
                           (cond ((null l) l)
                                 ((funcall p (car l))
                                  (cons (car l) (funcall filter p (cdr l))))
                                 (t (funcall filter p (cdr l)))))))
          (funcall filter
                   (lambda (f) (eq proc (frame-parameter f 'client)))
                   (frame-list))))))


;;; utilities

(defun other-client-frame (arg)
  "Select ARG-th other frame which belongs to other client.
Typical frame of each client is arranged in a cyclic order.
If multiple terminals exist, this cycles on same terminal
for current frame."
  (interactive "p")
  (let* ((client-list (terminal-client-list))
         (length (length client-list))
         (offset (letrec ((find-cdr
                           (lambda (p l)
                             (cond ((null l) l)
                                   ((funcall p (car l)) l)
                                   (t (funcall find-cdr p (cdr l)))))))
                   (- (length (funcall find-cdr
                                       (lambda (client)
                                         (eq client
                                             (frame-parameter nil 'client)))
                                       (reverse client-list)))
                      1)))
         (n (% (+ (% (+ arg offset) length) length) length))
         (client (nth n client-list)))
    (letrec ((find (lambda (p l)
                     (cond ((null l) l)
                           ((funcall p (car l)) (car l))
                           (t (funcall find p (cdr l)))))))
      (select-frame-set-input-focus
       (funcall find
                (lambda (frame) (eq (frame-parameter frame 'client) client))
                (frame-list))))))

(defun other-client-frame-reverse (arg)
  "`other-client-frame' by reverse order."
  (interactive "p")
  (other-client-frame (- arg)))

(defun other-frame-on-selected-client (arg)
  "Select ARG-th other frame which belongs to same client."
  (interactive "p")
  (let* ((frame-list (client-frame-list))
         (length (length frame-list))
         (offset (letrec ((find-cdr
                           (lambda (p l)
                             (cond ((null l) l)
                                   ((funcall p (car l)) l)
                                   (t (funcall find-cdr p (cdr l)))))))
                   (- (length (funcall find-cdr
                                       (lambda (frame)
                                         (eq frame (selected-frame)))
                                       (reverse frame-list)))
                      1)))
         (n (% (+ (% (+ arg offset) length) length) length))
         (frame (nth n frame-list)))
    (select-frame-set-input-focus frame)))

(defun other-frame-on-selected-client-reverse (arg)
  "`other-frame-on-selected-client' by reverse order."
  (interactive "p")
  (other-frame-on-selected-client (- arg)))

(defun increase-all-client-frames-alpha (&optional value)
  "Increase alpha value of all client frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (increase-frame-alpha value frame))
        (client-frame-list)))

(defun decrease-all-client-frames-alpha (&optional value)
  "Decrease alpha value of all client frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (decrease-frame-alpha value frame))
        (client-frame-list)))

(defun set-all-client-frames-alpha (value)
  "Set alpha value of all client frames by VALUE."
  (interactive "Nalpha: ")
  (mapc (lambda (frame) (set-frame-parameter frame 'alpha value))
        (client-frame-list)))

(defun toggle-all-client-frames-opacity ()
  "Toggle opacity of all client frames.
If current fram is opaque, set alpha of all frames as 0.
Otherwise, set 100."
  (interactive)
  (if (or (null (frame-parameter nil 'alpha))
          (= (frame-parameter nil 'alpha) 100))
      (mapc (lambda (frame) (set-frame-parameter frame 'alpha  0))
            (client-frame-list))
    (mapc (lambda (frame) (set-frame-parameter frame 'alpha  100))
          (client-frame-list))))


(resolve client)
