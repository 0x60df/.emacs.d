
;;;; client.el


(premise init)
(premise frame)
(premise bindings)


;;; subr

(defun server-client-list ()
  "Return list of clients."
  (let (proc-list)
    (mapc (lambda (frame)
            (let ((proc (frame-parameter frame 'client)))
              (if (and proc (not (memq proc proc-list)))
                  (push proc proc-list))))
          (frame-list))
    proc-list))

(defun server-client-frame-list (&optional client)
  "Return list of frames which belongs to the client of current frame.
If CLIENT is non-nil, use CLIEANT instead of the client of
current frame."
  (let* ((proc (or client (frame-parameter nil 'client)))
         (sift
          (if proc
              (lambda (fl)
                (letrec ((filter (lambda (p l)
                                   (cond ((null l) l)
                                         ((funcall p (car l))
                                          (cons (car l)
                                                (funcall filter p (cdr l))))
                                         (t (funcall filter p (cdr l)))))))
                  (funcall filter
                           (lambda (f) (eq proc (frame-parameter f 'client)))
                           fl)))
            (symbol-function 'identity))))
    (funcall sift (frame-list))))


;;; utilities

(defun server-other-client-frame (arg)
  "Select ARG-th other frame which belongs to other client.
Typical frame of each client is arranged in a cyclic order."
  (interactive "p")
  (let* ((client-list (server-client-list))
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

(defun server-other-client-frame-reverse (arg)
  "`server-other-client-frame' by reverse order."
  (interactive "p")
  (server-other-client-frame (- arg)))

(defun server-other-frame-on-selected-client (arg)
  "Select ARG-th other frame which belongs to same client."
  (interactive "p")
  (let* ((frame-list (server-client-frame-list))
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

(defun server-other-frame-on-selected-client-reverse (arg)
  "`server-other-frame-on-selected-client' by reverse order."
  (interactive "p")
  (server-other-frame-on-selected-client (- arg)))

(defun server-increase-all-client-frames-alpha (&optional value)
  "Increase alpha value of all client frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (increase-frame-alpha value frame))
        (server-client-frame-list)))

(defun server-decrease-all-client-frames-alpha (&optional value)
  "Decrease alpha value of all client frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (decrease-frame-alpha value frame))
        (server-client-frame-list)))

(defun server-set-all-client-frames-alpha (value)
  "Set alpha value of all client frames by VALUE."
  (interactive "Nalpha: ")
  (mapc (lambda (frame) (set-frame-parameter frame 'alpha value))
        (server-client-frame-list)))

(defun server-toggle-all-client-frames-opacity ()
  "Toggle opacity of all client frames.
If current fram is opaque, set alpha of all frames as 0.
Otherwise, set 100."
  (interactive)
  (if (or (null (frame-parameter nil 'alpha))
          (= (frame-parameter nil 'alpha) 100))
      (mapc (lambda (frame) (set-frame-parameter frame 'alpha  0))
            (server-client-frame-list))
    (mapc (lambda (frame) (set-frame-parameter frame 'alpha  100))
          (server-client-frame-list))))


;;; binding

(with-eval-after-load 'server
  (global-set-key (kbd "C-.") #'server-other-frame-on-selected-client)
  (global-set-key (kbd "C-M-.") #'server-other-frame-on-selected-client-reverse)
  (global-set-key (kbd "s-.") #'server-other-client-frame)
  (global-set-key (kbd "s-M-.") #'server-other-client-frame-reverse)
  (global-set-key (kbd "C-s-+") #'server-toggle-all-client-frames-opacity))


(resolve client)
