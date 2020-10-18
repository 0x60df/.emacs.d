
;;;; frame.el


(premise init)
(premise custom)


;;; terminal support

(defvar after-make-terminal-functions nil
  "Functions to run after `make-frame' created a new terminal.
The functions are run with one argument, the newly created
terminal, like `after-make-frame-functions'.")

(defun run-after-make-terminal-functions (make-frame &rest args)
  "Function for advising `make-frame'.
If new terminal is created by `make-frame',
`run-hook-with-args' `after-make-terminal-functions' with
newly created terminal."
  (let* ((existing-terminal-list (terminal-list))
         (made-frame (apply make-frame args))
         (made-frame-terminal (frame-terminal made-frame)))
    (unless (memq made-frame-terminal existing-terminal-list)
      (with-selected-frame made-frame
        (run-hook-with-args 'after-make-terminal-functions
                            made-frame-terminal)))
    made-frame))

(advice-add 'make-frame :around #'run-after-make-terminal-functions)


;;; utilities

(defun other-frame-reverse (arg)
  "Other frame by reverse order."
  (interactive "p")
  (other-frame (- arg)))

(defcustom frame-alpha-default-variation 10
  "Default value of increment or decrement for manipulating alpha."
  :type 'number
  :group 'user)

(defun increase-frame-alpha (&optional value frame)
  "Increase alpha value of the FRAME by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'.
If FRAME is nil or omitted, use current frame."
  (interactive "P")
  (set-frame-parameter
   frame
   'alpha
   (if (null (frame-parameter frame 'alpha))
       100
     (let ((result (+ (frame-parameter frame 'alpha)
                      (if value
                          (prefix-numeric-value value)
                        frame-alpha-default-variation))))
       (if (and (<= 0 result) (<= result 100))
           result
         (frame-parameter frame 'alpha))))))

(defun decrease-frame-alpha (&optional value frame)
  "Decrease alpha value of the FRAME by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'.
If FRAME is nil or omitted, use current frame."
  (interactive "P")
  (set-frame-parameter
   frame
   'alpha
   (if (null (frame-parameter frame 'alpha))
       100
     (let ((result (- (frame-parameter frame 'alpha)
                      (if value
                          (prefix-numeric-value value )
                        frame-alpha-default-variation))))
       (if (and (<= 0 result) (<= result 100))
           result
         (frame-parameter frame 'alpha))))))

(defun set-frame-alpha (value &optional frame)
  "Set alpha value of the FRAME by VALUE.
If FRAME is nil or omitted, use current frame."
  (interactive "Nalpha: ")
  (set-frame-parameter frame 'alpha value))

(defun toggle-frame-opacity (&optional frame)
  "Toggle opacity of the FRAME.
If FRAME is nil or omitted, use current frame.
If FRAME is opaque, set frame alpha as 0.
Otherwise, set 100."
  (interactive)
  (if (or (null (frame-parameter frame 'alpha))
          (= (frame-parameter frame 'alpha) 100))
      (set-frame-parameter frame 'alpha 0)
    (set-frame-parameter frame 'alpha 100)))

(defun increase-all-frames-alpha (&optional value)
  "Increase alpha value of all frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (increase-frame-alpha value frame))
        (frame-list)))

(defun decrease-all-frames-alpha (&optional value)
  "Decrease alpha value of all frames by VALUE.
If VALUE is nil or omitted, use
`frame-alpha-default-variation'."
  (interactive "P")
  (mapc (lambda (frame) (decrease-frame-alpha value frame))
        (frame-list)))

(defun set-all-frames-alpha (value)
  "Set alpha value of all frames by VALUE."
  (interactive "Nalpha: ")
  (modify-all-frames-parameters `((alpha . ,value))))

(defun toggle-all-frames-opacity ()
  "Toggle opacity of all frames.
If current fram is opaque, set alpha of all frames as 0.
Otherwise, set 100."
  (interactive)
  (if (or (null (frame-parameter nil 'alpha))
          (= (frame-parameter nil 'alpha) 100))
      (modify-all-frames-parameters '((alpha . 0)))
    (modify-all-frames-parameters '((alpha . 100)))))

(defcustom monitor-margin-alist nil
  "Monitor margin list.
This looks like '(pattern . (left top righ bottom)).
pattern is a list whose elements are compared with elements
of monitor attributes. If all elements of pattern are
matched, left top right bottom are used as margin for each
side of the monitor. Otherwise, margin is regarded as 0."
  :type 'alist
  :group 'user)

(defcustom manipulate-frame-default-factor 8
  "Defult factor of motion and deformation for manipulating frame."
  :type 'number
  :group 'user)

(defun manipulate-frame (&optional arg)
  "Manipulate frame position and size interactively."
  (interactive "P")
  (let* ((place-frame
          (lambda (frame x y)
            (modify-frame-parameters
             frame
             `((left . ,(if (< x 0) `(+ ,x) x))
               (top . ,(if (< y 0) `(+ ,y) y))))))
         (monitor-margin
          (lambda (monitor-attributes)
            (let ((margin-definition
                   (seq-find
                    (lambda (monitor-margin)
                      (seq-every-p
                       (lambda (pattern)
                         (seq-contains-p monitor-attributes pattern))
                       (car monitor-margin)))
                    monitor-margin-alist)))
              (if margin-definition
                  (cdr margin-definition)
                '(0 0 0 0)))))
         (frame (selected-frame))
         (factor (if arg
                     (prefix-numeric-value arg)
                   manipulate-frame-default-factor)))
    (catch 'quit
      (while t
        (let* ((workarea (frame-monitor-workarea))
               (margin (funcall monitor-margin (frame-monitor-attributes)))
               (left-border (+ (car workarea)
                               (car margin)))
               (top-border (+ (cadr workarea)
                              (cadr margin)))
               (right-border (- (+ (caddr workarea)
                                   (car workarea))
                                (caddr margin)))
               (bottom-border (- (+ (cadddr workarea)
                                    (cadr workarea))
                                 (cadddr margin)))
               (left-edge (cadr (assq 'outer-position (frame-geometry))))
               (top-edge (cddr (assq 'outer-position (frame-geometry)))))
          (cond ((< left-edge left-border)
                 (setq left-edge left-border))
                ((< right-border (+ left-edge (frame-outer-width)))
                 (setq left-edge (- right-border (frame-outer-width)))))
          (cond ((< top-edge top-border)
                 (setq top-edge top-border))
                ((< bottom-border (+ top-edge (frame-outer-height)))
                 (setq top-edge (- bottom-border (frame-outer-height)))))
          (funcall place-frame frame left-edge top-edge))
        (let* ((key-sequence (read-key-sequence-vector
                              (format
                               (concat "position[%04d,%04d]" " "
                                       "size[%04dx%04d]" " "
                                       "display[%04dx%04d]" " "
                                       "step[%02d]")
                               (cadr (assq 'outer-position (frame-geometry)))
                               (cddr (assq 'outer-position (frame-geometry)))
                               (frame-outer-width) (frame-outer-height)
                               (caddr (frame-monitor-workarea))
                               (cadddr (frame-monitor-workarea))
                               factor)))
               (key-description (key-description key-sequence))
               (key-binding (key-binding key-sequence))
               (workarea (frame-monitor-workarea))
               (margin (funcall monitor-margin (frame-monitor-attributes)))
               (left-border (+ (car workarea)
                               (car margin)))
               (top-border (+ (cadr workarea)
                              (cadr margin)))
               (right-border (- (+ (caddr workarea)
                                   (car workarea))
                                (caddr margin)))
               (bottom-border (- (+ (cadddr workarea)
                                    (cadr workarea))
                                 (cadddr margin)))
               (left-edge (cadr (assq 'outer-position (frame-geometry))))
               (top-edge (cddr (assq 'outer-position (frame-geometry)))))
          (cond ((equal key-description "f")
                 (funcall place-frame
                          frame
                          (let ((dest (+ left-edge
                                         (* factor (frame-char-width))))
                                (bound (- right-border (frame-outer-width))))
                            (if (< bound dest) bound dest))
                          top-edge))
                ((equal key-description "b")
                 (funcall place-frame
                          frame
                          (let ((dest (- left-edge
                                         (* factor (frame-char-width))))
                                (bound left-border))
                            (if (< dest bound) bound dest))
                          top-edge))
                ((equal key-description "n")
                 (funcall place-frame
                          frame
                          left-edge
                          (let ((dest (+ top-edge
                                         (* factor (frame-char-height))))
                                (bound (- bottom-border (frame-outer-height))))
                            (if (< bound dest) bound dest))))
                ((equal key-description "p")
                 (funcall place-frame
                          frame
                          left-edge
                          (let ((dest (- top-edge
                                         (* factor (frame-char-height))))
                                (bound top-border))
                            (if (< dest bound) bound dest))))
                ((equal key-description "a")
                 (funcall place-frame frame left-border top-edge))
                ((equal key-description "e")
                 (funcall place-frame
                          frame
                          (- right-border (frame-outer-width))
                          top-edge))
                ((equal key-description "M-f")
                 (funcall place-frame
                          frame
                          (let ((dest (+ left-edge (frame-outer-width)))
                                (bound (- right-border (frame-outer-width))))
                            (if (< bound dest) bound dest))
                          top-edge))
                ((equal key-description "M-b")
                 (funcall place-frame
                          frame
                          (let ((dest (- left-edge (frame-outer-width)))
                                (bound left-border))
                            (if (< dest bound) bound dest))
                          top-edge))
                ((equal key-description "<")
                 (funcall place-frame frame left-edge top-border))
                ((equal key-description ">")
                 (funcall place-frame
                          frame
                          left-edge
                          (- bottom-border (frame-outer-height))))
                ((equal key-description "v")
                 (funcall place-frame
                          frame
                          left-edge
                          (let ((dest (+ top-edge (frame-outer-height)))
                                (bound (- bottom-border (frame-outer-height))))
                            (if (< bound dest) bound dest))))
                ((equal key-description "M-v")
                 (funcall place-frame
                          frame
                          left-edge
                          (let ((dest (- top-edge (frame-outer-height)))
                                (bound top-border))
                            (if (< dest bound) bound dest))))
                ((equal key-description "C-f")
                 (set-frame-width frame (+ (frame-width) factor)))
                ((equal key-description "C-b")
                 (set-frame-width frame (- (frame-width) factor)))
                ((equal key-description "C-n")
                 (set-frame-height frame (+ (frame-height) factor)))
                ((equal key-description "C-p")
                 (set-frame-height frame (- (frame-height) factor)))
                ((equal key-description ".")
                 (letrec
                     ((dig
                       (lambda (p l)
                         (cond ((null l) (cons l nil))
                               ((funcall p (car l)) nil)
                               (t (cons (car l) (funcall dig p (cdr l))))))))
                   (let* ((monitor-attributes-list
                           (display-monitor-attributes-list))
                          (dug (funcall
                                dig
                                (lambda (monitor-attributes)
                                  (and (equal (cdr (assq 'geometry
                                                         monitor-attributes))
                                              (frame-monitor-geometry))
                                       (equal (cdr (assq 'workarea
                                                         monitor-attributes))
                                              (frame-monitor-workarea))))
                                monitor-attributes-list))
                          (monitor-attributes
                           (car (last (if (null dug)
                                          monitor-attributes-list
                                        dug))))
                          (workarea (cdr (assq 'workarea monitor-attributes)))
                          (margin (funcall monitor-margin monitor-attributes))
                          (left (- (+ (+ (car workarea) (car margin))
                                      (/ (- (caddr workarea)
                                            (car margin)
                                            (caddr margin))
                                         2))
                                   (/ (frame-outer-width) 2)))
                          (top (- (+ (+ (cadr workarea)  (cadr margin))
                                     (/ (- (cadddr workarea)
                                           (cadr margin)
                                           (cadddr margin))
                                        2))
                                  (/ (frame-outer-height) 2)))
                          (frame-workare (frame-monitor-workarea)))
                     (unless (and (and (< (car frame-workare) left)
                                       (< left (+ (car frame-workare)
                                                  (caddr frame-workare))))
                                  (and (< (cadr frame-workare) top)
                                       (< top (+ (cadr frame-workare)
                                                 (cadddr frame-workare)))))
                       (funcall place-frame frame left top)))))
                ((equal key-description "s")
                 (let ((read (read-from-minibuffer
                              (format "Step[%d]: " factor)
                              nil nil t nil (number-to-string factor))))
                   (if (integerp read) (setq factor read))))
                ((equal key-description "q")
                 (throw 'quit t))
                ((and (not (eq key-binding 'self-insert-command))
                      (commandp key-binding))
                 (call-interactively key-binding)
                 (throw 'quit t))
                (t (throw 'quit t))))))))

(defun raise-other-frame (arg)
  "Raise other frame which is selected interactively.
ARG-th frame is initially focused."
  (interactive "p")
  (let ((frame
         (letrec ((nth-frame
                   (lambda (n frame)
                     (cond ((zerop n) frame)
                           ((< 0 n)
                            (funcall nth-frame (- n 1) (next-frame frame)))
                           ((< n 0)
                            (funcall nth-frame (+ n 1) (previous-frame frame)))
                           (t frame)))))
           (funcall nth-frame arg (selected-frame))))
        (frame-alpha-alist
         (mapcar (lambda (frame) (cons frame (frame-parameter frame 'alpha)))
                 (frame-list))))
    (unwind-protect
        (progn
          (mapc (lambda (frame)
                  (set-frame-parameter frame 'alpha 0))
                (remove frame (frame-list)))
          (catch 'quit
            (while t
              (let* ((key-sequence (read-key-sequence-vector "Selected"))
                     (key-description (key-description key-sequence)))
                (cond ((equal key-description "n")
                       (set-frame-parameter frame 'alpha 0)
                       (setq frame (next-frame frame))
                       (set-frame-parameter frame 'alpha 100))
                      ((equal key-description "p")
                       (set-frame-parameter frame 'alpha 0)
                       (setq frame (previous-frame frame))
                       (set-frame-parameter frame 'alpha 100))
                      ((or (equal key-description "RET")
                           (equal key-description "C-j"))
                       (raise-frame frame)
                       (throw 'quit t))
                      (t (throw 'quit t)))))))
      (mapc (lambda (frame-alpha)
              (set-frame-parameter
               (car frame-alpha) 'alpha (or (cdr frame-alpha) 100)))
            frame-alpha-alist))))


(resolve frame)
