
;;;; frame.el


(premise init)
(premise custom)

(defun other-frame-reverse (arg)
  (interactive "p")
  (other-frame (- arg)))

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
      (run-hook-with-args 'after-make-terminal-functions made-frame-terminal))
    made-frame))

(advice-add 'make-frame :around #'run-after-make-terminal-functions)

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

(defcustom display-pixel-left-margin 0 "" :type 'integer :group 'user)
(defcustom display-pixel-top-margin 0 "" :type 'integer :group 'user)
(defcustom display-pixel-right-margin 0 "" :type 'integer :group 'user)
(defcustom display-pixel-bottom-margin 0 "" :type 'integer :group 'user)

(defun manipulate-frame (arg)
  (interactive "P")
  (let* ((left-border display-pixel-left-margin)
         (top-border display-pixel-top-margin)
         (right-border (- (display-pixel-width) display-pixel-right-margin))
         (bottom-border (- (display-pixel-height) display-pixel-bottom-margin))
         (frame (selected-frame))
         (d (or arg 1))
         l
         c)
    (let* ((left-edge (cdr (assoc 'left (frame-parameters))))
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
        (let ((left-edge (cdr (assoc 'left (frame-parameters))))
              (top-edge (cdr (assoc 'top (frame-parameters))))
              (d (if (null l)
                     d
                   (let ((digits (remq ?- (reverse l))))
                     (string-to-number
                      (concat (make-string
                               (% (- (length l) (length digits)) 2) ?-)
                              (or digits "1")))))))
          (setq c (read-char (format
                              (concat "position[%04d,%04d]" " "
                                      "size[%04dx%04d]" " "
                                      "display[%04dx%04d]" " "
                                      "step[%02d]")
                              left-edge top-edge
                              (frame-pixel-width) (frame-pixel-height)
                              (display-pixel-width) (display-pixel-height)
                              d)))
          (cond ((= c ?f)
                 (set-frame-position
                  frame
                  (let ((dest (+ left-edge (* d (frame-char-width))))
                        (bound (- right-border (frame-pixel-width))))
                    (if (< bound dest) bound dest))
                  top-edge))
                ((= c ?b)
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (* d (frame-char-width))))
                        (bound left-border))
                    (if (< dest bound) bound dest))
                  top-edge))
                ((= c ?n)
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (+ top-edge (* d (frame-char-height))))
                        (bound (- bottom-border (frame-pixel-height))))
                    (if (< bound dest) bound dest))))
                ((= c ?p)
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (* d (frame-char-height))))
                        (bound top-border))
                    (if (< dest bound) bound dest))))
                ((= c ?a)
                 (set-frame-position frame left-border top-edge))
                ((= c ?e)
                 (set-frame-position
                  frame
                  (- right-border (frame-pixel-width))
                  top-edge))
                ((= c 134217830)        ;M-f
                 (set-frame-position
                  frame
                  (let ((dest (+ left-edge (frame-pixel-width)))
                        (bound (- right-border (frame-pixel-width))))
                    (if (< bound dest) bound dest))
                  top-edge))
                ((= c 134217826)        ;M-b
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (frame-pixel-width)))
                        (bound left-border))
                    (if (< dest bound) bound dest))
                  top-edge))
                ((= c 134217788)        ;M-<
                 (set-frame-position frame left-edge top-border))
                ((= c 134217790)        ;M->
                 (set-frame-position
                  frame
                  left-edge
                  (- bottom-border (frame-pixel-height))))
                ((= c ?v)
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (+ top-edge (frame-pixel-height)))
                        (bound (- bottom-border (frame-pixel-height))))
                    (if (< bound dest) bound dest))))
                ((= c 134217846)        ;M-v
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (frame-pixel-height)))
                        (bound top-border))
                    (if (< dest bound) bound dest))))
                ((or (= c ?-) (and (<= ?0 c) (<= c ?9)))
                 (setq l (cons c l)))
                ((= c ?g)
                 (setq l '()))
                ((= c 6)                ;C-f
                 (set-frame-width frame (+ (frame-width) d)))
                ((= c 2)                ;C-b
                 (set-frame-width frame (- (frame-width) d)))
                ((= c 14)               ;C-n
                 (set-frame-height frame (+ (frame-height) d)))
                ((= c 16)               ;C-p
                 (set-frame-height frame (- (frame-height) d)))
                ((= c ?q)
                 (message "quit")
                 (throw 'quit t))))))))


(resolve frame)
