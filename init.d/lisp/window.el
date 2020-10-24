
;;;; window.el


(premise init)
(premise custom)

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


(resolve window)
