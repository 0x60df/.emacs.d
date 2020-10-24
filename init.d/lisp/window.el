
;;;; window.el


(premise init)
(premise custom)
(premise frame)

(defun other-window-reverse (arg)
  "Other window by reverse order."
  (interactive "p")
  (other-window (- arg)))

(defun split-window-above ()
  "Split window below and select it."
  (interactive)
  (let ((original-window (selected-window)))
    (select-window (call-interactively #'split-window-below))
    original-window))

(defun split-window-left ()
  "Split window right and select it."
  (interactive)
  (let ((original-window (selected-window)))
    (select-window (call-interactively #'split-window-right))
    original-window))

(defun split-window-below-or-right (&optional arg)
  "Split window below or right according to a prefix argument."
  (interactive "P")
  (if arg
      (select-window (split-window-right))
    (select-window (split-window-below))))

(defcustom manipulate-window-default-factor 2
  "Defult factor of deformation for manipulating window."
  :type 'number
  :group 'user)

(defun manipulate-window (&optional arg)
  "Manipulate window size interactively."
  (interactive "P")
  (let* ((initial-width (window-width))
         (initial-height (window-height))
         (initial-right-edge (nth 2 (window-pixel-edges)))
         (initial-bottom-edge (nth 3 (window-pixel-edges)))
         (direction-x (if (= initial-right-edge (frame-pixel-width))
                 -1
               1))
         (direction-y (if (= initial-bottom-edge
                    (- (frame-pixel-height)
                       (window-pixel-height (minibuffer-window))))
                 -1
               1))
         (factor (if arg
                     (prefix-numeric-value arg)
                   manipulate-window-default-factor)))
    (catch 'quit
      (while t
        (let* ((left-edge (nth 0 (window-edges)))
               (top-edge (nth 1 (window-edges)))
               (key-sequence (read-key-sequence-vector
                              (format
                               (concat "position[%02d,%02d]" " "
                                       "size[%02dx%02d]" " "
                                       "frame[%02dx%02d]" " "
                                       "step[%02d]")
                               left-edge top-edge
                               (window-width) (window-height)
                               (frame-width) (frame-height)
                               factor)))
               (key-description (key-description key-sequence))
               (key-binding (key-binding key-sequence)))
          (cond ((equal key-description "f")
                 (enlarge-window-horizontally (* factor direction-x)))
                ((equal key-description "b")
                 (shrink-window-horizontally (* factor direction-x)))
                ((equal key-description "n")
                 (enlarge-window (* factor direction-y)))
                ((equal key-description "p")
                 (shrink-window (* factor direction-y)))
                ((equal key-description "a")
                 (shrink-window-horizontally (* (frame-width) direction-x)))
                ((equal key-description "e")
                 (enlarge-window-horizontally (* (frame-width) direction-x)))
                ((equal key-description "M-f")
                 (enlarge-window-horizontally (* initial-width direction-x)))
                ((equal key-description "M-b")
                 (shrink-window-horizontally (* initial-width direction-x)))
                ((equal key-description "<")
                 (shrink-window (* (frame-height) direction-y)))
                ((equal key-description ">")
                 (enlarge-window (* (frame-height) direction-y)))
                ((equal key-description "v")
                 (enlarge-window (* initial-height direction-y)))
                ((equal key-description "M-v")
                 (shrink-window (* initial-height direction-y)))
                ((or (equal key-description "q")
                     (equal key-description "RET")
                     (equal key-description "C-j"))
                 (throw 'quit t))
                ((and (not (eq key-binding 'self-insert-command))
                      (commandp key-binding))
                 (call-interactively key-binding)
                 (throw 'quit t))
                (t (throw 'quit t))))))))

(defun view-other-window (&optional arg)
  "View buffer contents in other window.
If prefix argument ARG is specified, this function ask
window to use."
  (interactive "P")
  (let ((original-frame (selected-frame))
        (original-window (selected-window)))
    (unwind-protect
        (let* ((frame (if arg
                          (funcall-interactively #'pick-frame t)
                        original-frame))
               (window (if arg
                           (let* ((window-list
                                   (let ((raw-list (window-list frame)))
                                     (if (or (null frame)
                                             (eq frame original-frame))
                                         (remove original-window raw-list)
                                       raw-list)))
                                  (window-name-alist
                                   (mapcar
                                    (lambda (w) (cons (format "%s" w) w))
                                    window-list)))
                             (if (< 1 (length window-list))
                                 (prog2
                                     (raise-frame original-frame)
                                     (cdr (assoc
                                           (completing-read
                                            "Window: " window-name-alist nil t)
                                           window-name-alist))
                                   (raise-frame frame))
                               (car window-list)))
                         (if (< 1 (length (window-list)))
                             (other-window-for-scrolling)
                           (prog1
                               (split-window-above)
                             (setq original-window (selected-window)))))))
          (if (windowp window)
              (catch 'quit
                (while t
                  (let* ((key-sequence (read-key-sequence-vector
                                        "View other window:"))
                         (key-description (key-description key-sequence))
                         (key-binding (key-binding key-sequence)))
                    (with-selected-window window
                      (condition-case nil
                          (cond ((equal key-description ",")
                                 (let ((next-window (next-window window)))
                                   (setq window
                                         (if (eq next-window original-window)
                                             (next-window next-window)
                                           next-window))))
                                ((or (equal key-description "M-,")
                                     (equal key-description "."))
                                 (let ((previous-window
                                        (previous-window window)))
                                   (setq window
                                         (if (eq previous-window
                                                 original-window)
                                             (previous-window previous-window)
                                           previous-window))))
                                ((equal key-description "n") (scroll-up-line))
                                ((equal key-description "p") (scroll-down-line))
                                ((equal key-description "v") (scroll-up))
                                ((equal key-description "M-v") (scroll-down))
                                ((equal key-description "<")
                                 (goto-char (point-min)))
                                ((equal key-description ">")
                                 (goto-char (point-max)))
                                ((or (equal key-description "C-f")
                                     (equal key-description "f"))
                                 (unwind-protect
                                     (progn
                                       (redirect-frame-focus
                                        original-frame frame)
                                       (call-interactively #'find-file))
                                   (redirect-frame-focus original-frame nil)))
                                ((equal key-description "b")
                                 (unwind-protect
                                     (progn
                                       (redirect-frame-focus
                                        original-frame frame)
                                       (call-interactively #'switch-to-buffer))
                                   (redirect-frame-focus original-frame nil)))
                                ((or (equal key-description "q")
                                     (equal key-description "RET")
                                     (equal key-description "C-j"))
                                 (throw 'quit t))
                                ((and (not (eq key-binding
                                               'self-insert-command))
                                      (commandp key-binding))
                                 (with-selected-window original-window
                                   (let ((current-prefix-arg nil))
                                     (call-interactively key-binding)))
                                 (throw 'quit t))
                                (t (throw 'quit t)))
                        (beginning-of-buffer nil)
                        (end-of-buffer nil))))))))
      (if arg (raise-frame original-frame)))))


(resolve window)
