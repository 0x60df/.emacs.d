
;;;; cursor.el


(premise init)

(global-hl-line-mode)
(blink-cursor-mode 0)

(defcustom basic-cursor-color (face-attribute 'cursor :background)
  "Basic cursor color used in ordinay context.
This is fall-back point for manipulating cursor color in
running emacs session.
`reset-cursor-color' set cursor color as this variable."
  :group 'user
  :type 'color)

(defun reset-cursor-color (&optional frame)
  "Set cursor color as `basic-cursor-color'.
If optional argument FRAME is given, set cursor color of
that frame; otherwise, current frame is used."
  (interactive (if current-prefix-arg
                   (completing-read "Frame: " (frame-list)
                                    (lambda (frame)
                                      (eq (frame-parameter frame 'display-type)
                                          'color))
                                    t)))
  (set-frame-parameter frame 'cursor-color basic-cursor-color))


(resolve cursor)
