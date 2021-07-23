
;;;; frame.el


(premise init)
(premise custom)
(premise feature)

(lazy-autoload 'seq-every-p "seq")
(lazy-autoload 'seq-contains-p "seq")
(lazy-autoload 'frameset-name "frameset")
(lazy-autoload 'frameset-description "frameset")


;;; settings

(custom-set-variables
 '(x-frame-normalize-before-maximize t))



;;; redirect event

(defcustom keeping-time-for-frame-focus-redirection-on-make-frame 0.3
  "Keeping time for the redirection for the moment of make frame."
  :type 'float
  :group 'user)

(defun set-frame-focus-redirection-on-make-frame (frame)
  "Redirect frame focus for the moment of make frame.
This function work with `after-make-frame-functions'.
Redirection is canceled after "
  (let ((base (selected-frame)))
    (redirect-frame-focus base frame)
    (run-with-timer keeping-time-for-frame-focus-redirection-on-make-frame
                    nil
                    #'redirect-frame-focus base nil)))

(add-hook 'after-make-frame-functions
          #'set-frame-focus-redirection-on-make-frame)

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



;;; session

(defcustom frame-session-max 16
  "Maximum number of `frame-session-list'."
  :type 'integer
  :group 'user)

(defvar frame-session-list nil "List to store frame sessions.")

(defun frame-save-session (&optional frame)
  "Save current session for FRAME as a `frameset'.
If FRAME is not specified, use current frame.
Session will be saved in `frame-session-list'."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (buffer-list (mapcar #'window-buffer (window-list frame)))
         (buffer-num (length buffer-list))
         (frameset
          (frameset-save (list frame)
                         :app 'user
                         :name (format "%s" (frame-parameter frame 'window-id))
                         :description (format "[%s] %d buffer%s: %s"
                                              (format-time-string "%FT%T")
                                              buffer-num
                                              (if (not (eql buffer-num 1))
                                                  "s"
                                                "")
                                              (mapconcat #'buffer-name
                                                         buffer-list
                                                         ", ")))))
    (let ((session-num (length frame-session-list)))
      (setq frame-session-list
            (cons frameset
                  (if (< session-num frame-session-max)
                      frame-session-list
                    (butlast frame-session-list
                             (+ 1 (- session-num frame-session-max)))))))))

(defun frame-restore-session (session)
  "Restore frame session by SESSION.
SESSION is a `frameset'.
When interactively, ask SESSION from `frame-session-list'."
  (interactive (list
                (let ((name-session-alist
                       (mapcar (lambda (session)
                                 (cons (format "%s %s"
                                               (frameset-name session)
                                               (frameset-description session))
                                       session))
                               frame-session-list)))
                  (cdr (assoc (completing-read "Session: "
                                               name-session-alist nil t)
                              name-session-alist)))))
  (let ((selected-frame (selected-frame)))
    (frameset-restore session
                      :reuse-frames
                      (lambda (frame)
                        (eq selected-frame frame)))))

(add-hook 'delete-frame-functions #'frame-save-session)



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

(defvar default-frame-size `(,(frame-width) . ,(frame-height))
  "Defaul value of frame width and height.")

(add-hook 'after-make-terminal-functions
          (lambda (terminal)
            (setq default-frame-size `(,(frame-width) . ,(frame-height)))))

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

(defcustom switch-monitor-delay 0
  "Delay time in milliseconds for switching monitor."
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
               (top-edge (cddr (assq 'outer-position (frame-geometry))))
               frame-out-of-border)
          (cond ((< left-edge left-border)
                 (setq left-edge left-border)
                 (setq frame-out-of-border t))
                ((< right-border (+ left-edge (frame-outer-width)))
                 (setq left-edge (- right-border (frame-outer-width)))
                 (setq frame-out-of-border t)))
          (cond ((< top-edge top-border)
                 (setq top-edge top-border)
                 (setq frame-out-of-border t))
                ((< bottom-border (+ top-edge (frame-outer-height)))
                 (setq top-edge (- bottom-border (frame-outer-height)))
                 (setq frame-out-of-border t)))
          (if frame-out-of-border
              (funcall place-frame frame left-edge top-edge)))
        (let* ((key-sequence (read-key-sequence-vector
                              (format
                               (concat "#<"
                                       (propertize "frame" 'face 'shadow)
                                       " "
                                       "width[%03d]" " "
                                       "height[%03d]" " "
                                       "step[%02d]"
                                       ">")
                               (frame-width) (frame-height) factor)))
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
                ((equal key-description "w")
                 (let ((read (read-from-minibuffer
                              (format "Width[%d]: " (frame-width))
                              nil nil t nil (number-to-string (frame-width)))))
                   (if (integerp read)
                       (set-frame-width frame read))))
                ((equal key-description "h")
                 (let ((read (read-from-minibuffer
                              (format "Height[%d]: " (frame-height))
                              nil nil t nil (number-to-string (frame-height)))))
                   (if (integerp read)
                       (set-frame-height frame read))))
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
                          (frame-workarea (frame-monitor-workarea)))
                     (unless (and (and (< (car frame-workarea) left)
                                       (< left (+ (car frame-workarea)
                                                  (caddr frame-workarea))))
                                  (and (< (cadr frame-workarea) top)
                                       (< top (+ (cadr frame-workarea)
                                                 (cadddr frame-workarea)))))
                       (funcall place-frame frame left top)
                       (sleep-for 0 switch-monitor-delay)))))
                ((equal key-description "s")
                 (let ((read (read-from-minibuffer
                              (format "Step[%d]: " factor)
                              nil nil t nil (number-to-string factor))))
                   (if (integerp read) (setq factor read))))
                ((equal key-description ";")
                 (modify-frame-parameters
                  frame
                  `((width . ,(car default-frame-size))
                    (height . ,(cdr default-frame-size)))))
                ((or (equal key-description "q")
                     (equal key-description "RET")
                     (equal key-description "C-j"))
                 (throw 'quit t))
                ((eq key-binding 'self-insert-command)
                 (if (< 0 (length key-sequence))
                     (let ((character (aref key-sequence 0)))
                       (if (characterp character)
                           (self-insert-command 1 character))))
                 (throw 'quit t))
                ((commandp key-binding)
                 (call-interactively key-binding)
                 (throw 'quit t))
                (t (throw 'quit t))))))))

(defun pick-frame (&optional post-process)
  "Choose frame interactively and return it.
POST-PROCESS specifies how to deal with chosen frame.
`focus' or 1   : `select-frame-set-input-focus'.
`raise' or 4   : `raise-frame'.
`delete' or 16 : `delete-frame'.
Otherwise: do nothing.
Thus, interactive call without prefix arguments focus frame,
and raise frame with prefix arguments."
  (interactive "p")
  (letrec ((frame (selected-frame))
           (frame-alpha-alist
            (mapcar (lambda (frame) (cons frame (frame-parameter frame 'alpha)))
                    (frame-list)))
           (most (lambda (p m l)
                   (cond ((null l) m)
                         ((funcall p (car l) m)
                          (funcall most p (car l) (cdr l)))
                         (t (funcall most p m (cdr l)))))))
    (when (< 1 (length (frame-list)))
      (unwind-protect
          (progn
            (mapc (lambda (frame)
                    (set-frame-parameter frame 'alpha 0))
                  (remove frame (frame-list)))
            (catch 'quit
              (while t
                (let* ((key-sequence (read-key-sequence-vector ""))
                       (key-description (key-description key-sequence))
                       (key-binding (key-binding key-sequence)))
                  (cond ((equal key-description ".")
                         (set-frame-parameter frame 'alpha 0)
                         (setq frame (next-frame frame))
                         (set-frame-parameter frame 'alpha 100))
                        ((or (equal key-description "M-.")
                             (equal key-description ","))
                         (set-frame-parameter frame 'alpha 0)
                         (setq frame (previous-frame frame))
                         (set-frame-parameter frame 'alpha 100))
                        ((equal key-description "f")
                         (let ((right-frame-list
                                (seq-filter
                                 (lambda (f)
                                   (let ((test-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (test-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (base-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry frame))))
                                         (base-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry frame)))))
                                     (or (< base-left test-left)
                                         (and (= base-left test-left)
                                              (< base-top test-top)))))
                                 (frame-list))))
                           (unless (null right-frame-list)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame
                                   (funcall
                                    most
                                    (lambda (try now)
                                      (let ((try-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (try-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (now-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry now))))
                                            (now-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry now)))))
                                        (or (< try-left now-left)
                                            (and (= try-left now-left)
                                                 (< try-top now-top)))))
                                    (car right-frame-list)
                                    right-frame-list))
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "b")
                         (let ((left-frame-list
                                (seq-filter
                                 (lambda (f)
                                   (let ((test-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (test-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (base-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry frame))))
                                         (base-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry frame)))))
                                     (or (< test-left base-left)
                                         (and (= test-left base-left)
                                              (< test-top base-top)))))
                                 (frame-list))))
                           (unless (null left-frame-list)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame
                                   (funcall
                                    most
                                    (lambda (try now)
                                      (let ((try-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (try-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (now-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry now))))
                                            (now-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry now)))))
                                        (or (< now-left try-left)
                                            (and (= now-left try-left)
                                                 (< now-top try-top)))))
                                    (car left-frame-list)
                                    left-frame-list))
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "n")
                         (let ((below-frame-list
                                (seq-filter
                                 (lambda (f)
                                   (let ((test-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (test-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (base-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry frame))))
                                         (base-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry frame)))))
                                     (or (< base-top test-top)
                                         (and (= base-top test-top)
                                              (< base-left test-left)))))
                                 (frame-list))))
                           (unless (null below-frame-list)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame
                                   (funcall
                                    most
                                    (lambda (try now)
                                      (let ((try-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (try-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (now-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry now))))
                                            (now-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry now)))))
                                        (or (< try-top now-top)
                                            (and (= try-top now-top)
                                                 (< try-left now-left)))))
                                    (car below-frame-list)
                                    below-frame-list))
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "p")
                         (let ((above-frame-list
                                (seq-filter
                                 (lambda (f)
                                   (let ((test-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (test-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry f))))
                                         (base-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry frame))))
                                         (base-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry frame)))))
                                     (or (< test-top base-top)
                                         (and (= test-top base-top)
                                              (< test-left base-left)))))
                                 (frame-list))))
                           (unless (null above-frame-list)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame
                                   (funcall
                                    most
                                    (lambda (try now)
                                      (let ((try-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (try-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry try))))
                                            (now-left
                                             (cadr (assq
                                                    'outer-position
                                                    (frame-geometry now))))
                                            (now-top
                                             (cddr (assq
                                                    'outer-position
                                                    (frame-geometry now)))))
                                        (or (< now-top try-top)
                                            (and (= now-top try-top)
                                                 (< now-left try-left)))))
                                    (car above-frame-list)
                                    above-frame-list))
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "a")
                         (let ((left-most-frame
                                (funcall
                                 most
                                 (lambda (try now)
                                   (let ((try-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (try-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (now-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry now))))
                                         (now-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry now)))))
                                     (or (< try-left now-left)
                                         (and (= try-left now-left)
                                              (< try-top now-top)))))
                                 (car (frame-list))
                                 (frame-list))))
                           (unless (eq left-most-frame frame)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame left-most-frame)
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "e")
                         (let ((right-most-frame
                                (funcall
                                 most
                                 (lambda (try now)
                                   (let ((try-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (try-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (now-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry now))))
                                         (now-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry now)))))
                                     (or (< now-left try-left)
                                         (and (= now-left try-left)
                                              (< now-top try-top)))))
                                 (car (frame-list))
                                 (frame-list))))
                           (unless (eq right-most-frame frame)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame right-most-frame)
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description "<")
                         (let ((top-most-frame
                                (funcall
                                 most
                                 (lambda (try now)
                                   (let ((try-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (try-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (now-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry now))))
                                         (now-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry now)))))
                                     (or (< try-top now-top)
                                         (and (= try-top now-top)
                                              (< try-left now-left)))))
                                 (car (frame-list))
                                 (frame-list))))
                           (unless (eq top-most-frame frame)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame top-most-frame)
                             (set-frame-parameter frame 'alpha 100))))
                        ((equal key-description ">")
                         (let ((bottom-most-frame
                                (funcall
                                 most
                                 (lambda (try now)
                                   (let ((try-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (try-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry try))))
                                         (now-left
                                          (cadr (assq
                                                 'outer-position
                                                 (frame-geometry now))))
                                         (now-top
                                          (cddr (assq
                                                 'outer-position
                                                 (frame-geometry now)))))
                                     (or (< now-top try-top)
                                         (and (= now-top try-top)
                                              (< now-left try-left)))))
                                 (car (frame-list))
                                 (frame-list))))
                           (unless (eq bottom-most-frame frame)
                             (set-frame-parameter frame 'alpha 0)
                             (setq frame bottom-most-frame)
                             (set-frame-parameter frame 'alpha 100))))
                        ((or (equal key-description "RET")
                             (equal key-description "C-j"))
                         (cond ((or (eq post-process 'focus)
                                    (eql post-process 1))
                                (select-frame-set-input-focus frame))
                               ((or (eq post-process 'raise)
                                    (eql post-process 4))
                                (raise-frame frame))
                               ((or (eq post-process 'delete)
                                    (eql post-process 16))
                                (delete-frame frame)))
                         (throw 'quit frame))
                        ((equal key-description "q")
                         (throw 'quit nil))
                        ((eq key-binding 'self-insert-command)
                         (if (< 0 (length key-sequence))
                             (let ((character (aref key-sequence 0)))
                               (if (characterp character)
                                   (self-insert-command 1 character))))
                         (throw 'quit nil))
                        ((commandp key-binding)
                         (call-interactively key-binding)
                         (throw 'quit nil))
                        (t (throw 'quit nil)))))))
        (mapc (lambda (frame-alpha)
                (let ((frame (car frame-alpha))
                      (alpha (cdr frame-alpha)))
                  (if (frame-live-p frame)
                      (set-frame-parameter frame 'alpha (or alpha 100)))))
              frame-alpha-alist)))))

(defvar switch-frame-alists nil "List of frame alist for switching.")
(make-variable-buffer-local 'switch-frame-alists)

(defun switch-frame-alist (&optional n)
  "Switch frame alist among `switch-frame-alists'."
  (interactive "p")
  (if switch-frame-alists
      (let* ((parameters (frame-parameters))
             (rest (seq-drop-while
                    (lambda (alist)
                      (not (seq-every-p
                            (lambda (key-value)
                              (equal (cdr
                                      (assq
                                       (let ((key (car key-value)))
                                         (cond ((eq key 'font) 'font-parameter)
                                               (t key)))
                                       parameters))
                                     (cdr key-value)))
                            alist)))
                    switch-frame-alists))
             (length (length switch-frame-alists))
             (offset (- length (length rest))))
        (modify-frame-parameters
         nil
         (nth (if rest (% (+ (% (+ n offset) length) length) length) 1)
              switch-frame-alists)))))

(defun switch-frame-alist-reverse (&optional n)
  "`switch-frame-alist' by reverse direction."
    (interactive "p")
    (switch-frame-alist (- n)))


(resolve frame)
