
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

(defun filter-typical-frame-of-each-client (frame-list)
  "Advising fuction for `frame-list'.
Filter FRAME-LIST and return list of typical frames of
each client."
  (letrec ((filter-first-frame-of-each-client
            (lambda (frame-list client-list)
                     (cond ((null frame-list) frame-list)
                           ((let ((client (frame-parameter
                                           (car frame-list) 'client)))
                              (or (null client) (memq client client-list)))
                            (funcall filter-first-frame-of-each-client
                                     (cdr frame-list) client-list))
                           (t (cons (car frame-list)
                                    (funcall filter-first-frame-of-each-client
                                             (cdr frame-list)
                                             (cons (frame-parameter
                                                    (car frame-list) 'client)
                                                   client-list))))))))
    (funcall filter-first-frame-of-each-client frame-list nil)))

(defun seek-nearest-typical-frame-of-each-client
    (next-or-previous-frame &optional frame miniframe args)
  "Advising function for `next-frame' and `previous-frame'.
Call these function recursively while returned frame
belongs to the same client as originally selected frame."
  (let ((frame-list-is-adviced t))
    (unwind-protect
        (progn
          (unless (advice-member-p #'filter-typical-frame-of-each-client
                                   'frame-list)
            (setq frame-list-is-adviced nil)
            (advice-add 'frame-list
                        :filter-return #'filter-typical-frame-of-each-client))
          (let ((original-frame (selected-frame))
                (typical-frame-list (frame-list)))
            (letrec ((seek (lambda (f)
                             (cond ((eq original-frame f) f)
                                   ((memq f typical-frame-list) f)
                                   (t (funcall seek (apply
                                                     next-or-previous-frame
                                                     f miniframe args)))))))
              (funcall seek (apply next-or-previous-frame
                                   frame miniframe args)))))
      (unless (and frame-list-is-adviced
                   (advice-member-p #'filter-typical-frame-of-each-client
                                    'frame-list)
        (advice-remove 'frame-list #'filter-typical-frame-of-each-client))))))

(defun pick-typical-frame-of-each-client ()
  "Pick typical frame of each client."
  (interactive)
  (let ((frame-alpha-alist
         (mapcar (lambda (frame) (cons frame (frame-parameter frame 'alpha)))
                 (frame-list))))
    (when (< 1 (length (terminal-client-list)))
      (unwind-protect
          (let* ((typical-frame (car (client-frame-list
                                      (frame-parameter
                                       (selected-frame) 'client)))))
            (mapc (lambda (frame)
                    (set-frame-parameter frame 'alpha 0))
                  (remove typical-frame (frame-list)))
            (select-frame-set-input-focus typical-frame)
            (advice-add 'frame-list
                        :filter-return #'filter-typical-frame-of-each-client)
            (advice-add 'next-frame
                        :around #'seek-nearest-typical-frame-of-each-client)
            (advice-add 'previous-frame
                        :around #'seek-nearest-typical-frame-of-each-client)
            (call-interactively #'pick-frame))
        (advice-remove 'previous-frame
                       #'seek-nearest-typical-frame-of-each-client)
        (advice-remove 'next-frame #'seek-nearest-typical-frame-of-each-client)
        (advice-remove 'frame-list #'filter-typical-frame-of-each-client)
        (mapc (lambda (frame-alpha)
                (set-frame-parameter
                 (car frame-alpha) 'alpha (or (cdr frame-alpha) 100)))
              frame-alpha-alist)))))

(defun filter-frame-on-selected-client (frame-list)
  "Advising fuction for `frame-list'.
Filter FRAME-LIST and return list of frames which belongs
to the client of current frame."
  (let* ((proc (frame-parameter nil 'client)))
    (if proc
        (letrec ((filter (lambda (p l)
                           (cond ((null l) l)
                                 ((funcall p (car l))
                                  (cons (car l) (funcall filter p (cdr l))))
                                 (t (funcall filter p (cdr l)))))))
          (funcall filter
                   (lambda (f) (eq proc (frame-parameter f 'client)))
                   frame-list)))))

(defun seek-nearest-frame-on-selected-frame
    (next-or-previous-frame &optional frame miniframe args)
  "Advising function for `next-frame' and `previous-frame'.
Call these function recursively while returned frame
belongs the other client than originally selected frame."
  (let* ((original-frame (selected-frame))
         (original-client (frame-parameter original-frame 'client)))
    (letrec ((seek (lambda (f)
                     (cond ((eq original-frame f) f)
                           ((eq original-client (frame-parameter f 'client)) f)
                           (t (funcall seek (apply next-or-previous-frame
                                                   f miniframe args)))))))
      (funcall seek (apply next-or-previous-frame frame miniframe args)))))

(defun pick-frame-on-selected-client ()
  "Pick frame which belongs to same client."
  (interactive)
  (let ((frame-alpha-alist
         (mapcar (lambda (frame) (cons frame (frame-parameter frame 'alpha)))
                 (frame-list))))
    (when (< 1 (length (client-frame-list)))
      (unwind-protect
          (progn
            (mapc (lambda (frame)
                    (set-frame-parameter frame 'alpha 0))
                  (remove (selected-frame) (frame-list)))
            (advice-add 'frame-list
                        :filter-return #'filter-frame-on-selected-client)
            (advice-add 'next-frame
                        :around #'seek-nearest-frame-on-selected-frame)
            (advice-add 'previous-frame
                        :around #'seek-nearest-frame-on-selected-frame)
            (call-interactively #'pick-frame))
        (advice-remove 'frame-list #'filter-frame-on-selected-client)
        (advice-remove 'next-frame #'seek-nearest-frame-on-selected-frame)
        (advice-remove 'previous-frame #'seek-nearest-frame-on-selected-frame)
        (mapc (lambda (frame-alpha)
                (set-frame-parameter
                 (car frame-alpha) 'alpha (or (cdr frame-alpha) 100)))
              frame-alpha-alist)))))


(resolve client)
