
;;;; functions.el


(defun duplicate-and-comment-out ()
  (interactive)
  (if (region-active-p)
      (let* ((rb (region-beginning))
             (re (region-end))
             (lb (save-excursion (goto-char rb) (line-beginning-position)))
             (le (save-excursion (goto-char re) (line-end-position))))
        (when (and (eq rb lb) (eq re le))
          (goto-char rb)
          (kill-ring-save rb re)
          (comment-region rb re)
          (yank)
          (insert "\n")
          (goto-char rb)))))

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun next-line-recenter()
  (interactive)
  (forward-line)
  (recenter))
(defun previous-line-recenter()
  (interactive)
  (forward-line -1)
  (recenter))

(defun manipulate-frame (arg)
  (interactive "P")
  (let* ((frame (selected-frame))
         (d (or arg 1))
         l
         c)
    (catch 'quit
      (while t
        (let ((left-edge (cdr (assoc 'left (frame-parameters))))
              (top-edge (cdr (assoc 'top (frame-parameters))))
              (d (if (null l)
                     d
                   (let ((digits (remq ?- (reverse l))))
                     (string-to-number (concat
                                     (make-string
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
                  frame (+ left-edge (* d (frame-char-width))) top-edge))
                ((= c ?b)
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (* d (frame-char-width)))))
                    (if (< dest 0) 0 dest))
                  top-edge))
                ((= c ?n)
                 (set-frame-position
                  frame left-edge (+ top-edge (* d (frame-char-height)))))
                ((= c ?p)
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (* d (frame-char-height)))))
                    (if (< dest 0) 0 dest))))
                ((= c ?a)
                 (set-frame-position frame 0 top-edge))
                ((= c ?e)
                 (set-frame-position
                  frame
                  (- (display-pixel-width) (frame-pixel-width))
                  top-edge))
                ((= c 134217830)        ;M-f
                 (set-frame-position
                  frame (+ left-edge (frame-pixel-width)) top-edge))
                ((= c 134217826)        ;M-b
                 (set-frame-position
                  frame
                  (let ((dest (- left-edge (frame-pixel-width))))
                    (if (< dest 0) 0 dest))
                  top-edge))
                ((= c 134217788)        ;M-<
                 (set-frame-position frame left-edge 0))
                ((= c 134217790)        ;M->
                 (set-frame-position
                  frame
                  left-edge
                  (- (display-pixel-height) (frame-pixel-height))))
                ((= c ?v)
                 (set-frame-position
                  frame left-edge (+ top-edge (frame-pixel-height))))
                ((= c 134217846)        ;M-v
                 (set-frame-position
                  frame
                  left-edge
                  (let ((dest (- top-edge (frame-pixel-height))))
                    (if (< dest 0) 0 dest))))
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

(defun manipulate-window (arg)
  (interactive "P")
  (let* ((initial-width (window-width))
         (initial-height (window-height))
         (initial-right-edge (nth 2 (window-pixel-edges)))
         (initial-bottom-edge (nth 3 (window-pixel-edges)))
         (ex (if (= initial-right-edge (frame-pixel-width)) -1 1))
         (ey (if (= initial-bottom-edge
                    (- (frame-pixel-height)
                       (window-pixel-height (minibuffer-window)))) -1 1))
         (r (or arg 1))
         l
         c)
    (catch 'quit
      (while t
        (let ((left-edge (nth 0 (window-edges)))
              (top-edge (nth 1 (window-edges)))
              (r (if (null l)
                     r
                   (let ((digits (remq ?- (reverse l))))
                     (string-to-number (concat
                                     (make-string
                                      (% (- (length l) (length digits)) 2) ?-)
                                     (or digits "1")))))))
          (setq c (read-char (format
                              (concat "position[%02d,%02d]" " "
                                      "size[%02dx%02d]" " "
                                      "frame[%02dx%02d]" " "
                                      "step[%02d]")
                              left-edge top-edge
                              (window-width) (window-height)
                              (frame-width) (frame-height)
                              r)))
          (cond ((= c ?f)
                 (enlarge-window-horizontally (* r ex)))
                ((= c ?b)
                 (shrink-window-horizontally (* r ex)))
                ((= c ?n)
                 (enlarge-window (* r ey)))
                ((= c ?p)
                 (shrink-window (* r ey)))
                ((= c ?a)
                 (shrink-window-horizontally (* (frame-width) ex)))
                ((= c ?e)
                 (enlarge-window-horizontally (* (frame-width) ex)))
                ((= c 134217830)        ;M-f
                 (enlarge-window-horizontally (* initial-width ex)))
                ((= c 134217826)        ;M-b
                 (shrink-window-horizontally (* initial-width ex)))
                ((= c 134217788)        ;M-<
                 (shrink-window (* (frame-height) ey)))
                ((= c 134217790)        ;M->
                 (enlarge-window (* (frame-height) ey)))
                ((= c ?v)
                 (enlarge-window (* initial-height ey)))
                ((= c 134217846)        ;M-v
                 (shrink-window (* initial-height ey)))
                ((or (= c ?-) (and (<= ?0 c) (<= c ?9)))
                 (setq l (cons c l)))
                ((= c ?g)
                 (setq l '()))
                ((= c ?q)
                 (message "quit")
                 (throw 'quit t))))))))

(defun split-window-below-or-right (arg)
  (interactive "P")
  (if arg
      (select-window (split-window-right))
    (select-window (split-window-below))))
