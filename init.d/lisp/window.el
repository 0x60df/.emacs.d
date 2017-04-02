
;;;; window.el


(premise init)

(defun split-window-below-or-right (arg)
  (interactive "P")
  (if arg
      (select-window (split-window-right))
    (select-window (split-window-below))))

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


(resolve window)
