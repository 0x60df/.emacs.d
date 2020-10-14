
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

(defcustom display-pixel-left-margin 0
  "Display margin on left side by pixelwise.
`manipulate-frame' keeps frame in this margin."
  :type 'integer
  :group 'user)

(defcustom display-pixel-top-margin 0
  "Display margin on top side by pixelwise.
`manipulate-frame' keeps frame in this margin."
  :type 'integer
  :group 'user)

(defcustom display-pixel-right-margin 0
  "Display margin on right side by pixelwise.
`manipulate-frame' keeps frame in this margin."
  :type 'integer
  :group 'user)

(defcustom display-pixel-bottom-margin 0
  "Display margin on bottom side by pixelwise.
`manipulate-frame' keeps frame in this margin."
  :type 'integer
  :group 'user)

(defcustom manipulate-frame-default-factor 8
  "Defult factor of motion and deformation for manipulating frame."
  :type 'number
  :group 'user)

(defun manipulate-frame (&optional arg)
  "Manipulate frame position and size interactively."
  (interactive "P")
  (let* ((left-border display-pixel-left-margin)
         (top-border display-pixel-top-margin)
         (right-border (- (display-pixel-width) display-pixel-right-margin))
         (bottom-border (- (display-pixel-height) display-pixel-bottom-margin))
         (frame (selected-frame))
         (factor (if arg
                        (prefix-numeric-value arg)
                      manipulate-frame-default-factor)))
    (let ((left-edge (cdr (assoc 'left (frame-parameters))))
          (top-edge (cdr (assoc 'top (frame-parameters)))))
      (cond ((or (listp left-edge)
                 (< left-edge left-border))
             (set-frame-position frame left-border top-edge))
            ((< right-border (+ left-edge (frame-pixel-width)))
             (set-frame-position frame
                                 (- right-border (frame-pixel-width))
                                 top-edge)))
      (cond ((or (listp top-edge)
                 (< top-edge top-border))
             (set-frame-position frame left-edge top-border))
            ((< bottom-border (+ top-edge (frame-pixel-height)))
             (set-frame-position frame
                                 left-edge
                                 (- bottom-border (frame-pixel-height))))))
    (catch 'quit
      (while t
        (let* ((left-edge (cdr (assoc 'left (frame-parameters))))
               (top-edge (cdr (assoc 'top (frame-parameters))))
               (key-sequence (read-key-sequence-vector
                              (format
                               (concat "position[%04d,%04d]" " "
                                       "size[%04dx%04d]" " "
                                       "display[%04dx%04d]" " "
                                       "step[%02d]")
                               left-edge top-edge
                               (frame-pixel-width) (frame-pixel-height)
                               (display-pixel-width) (display-pixel-height)
                               factor)))
               (key-description (key-description key-sequence))
               (key-binding (key-binding key-sequence)))
          (cond ((equal key-description "f")
                 (set-frame-position
                  frame
                  (let ((dest (+ left-edge (* factor (frame-char-width))))
                        (bound (- right-border (frame-pixel-width))))
                    (if (< bound dest) bound dest))
                  top-edge))
                ((equal key-description "b")
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (* factor (frame-char-width))))
                        (bound left-border))
                    (if (< dest bound) bound dest))
                  top-edge))
                ((equal key-description "n")
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (+ top-edge (* factor (frame-char-height))))
                        (bound (- bottom-border (frame-pixel-height))))
                    (if (< bound dest) bound dest))))
                ((equal key-description "p")
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (* factor (frame-char-height))))
                        (bound top-border))
                    (if (< dest bound) bound dest))))
                ((equal key-description "a")
                 (set-frame-position frame left-border top-edge))
                ((equal key-description "e")
                 (set-frame-position
                  frame
                  (- right-border (frame-pixel-width))
                  top-edge))
                ((equal key-description "M-f")
                 (set-frame-position
                  frame
                  (let ((dest (+ left-edge (frame-pixel-width)))
                        (bound (- right-border (frame-pixel-width))))
                    (if (< bound dest) bound dest))
                  top-edge))
                ((equal key-description "M-b")
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (frame-pixel-width)))
                        (bound left-border))
                    (if (< dest bound) bound dest))
                  top-edge))
                ((equal key-description "<")
                 (set-frame-position frame left-edge top-border))
                ((equal key-description ">")
                 (set-frame-position
                  frame
                  left-edge
                  (- bottom-border (frame-pixel-height))))
                ((equal key-description "v")
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (+ top-edge (frame-pixel-height)))
                        (bound (- bottom-border (frame-pixel-height))))
                    (if (< bound dest) bound dest))))
                ((equal key-description "M-v")
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (frame-pixel-height)))
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
