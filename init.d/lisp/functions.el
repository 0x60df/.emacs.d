
;;;; functions.el


(premise init)

(defun duplicate-and-comment ()
  "Duplicate region and comment out them."
  (interactive)
  (if (region-active-p)
      (let* ((p (point))
             (rb (region-beginning))
             (re (region-end)))
        (goto-char rb)
        (kill-ring-save rb re)
        (comment-region rb re)
        (yank)
        (newline-and-indent)
        (goto-char p))))

(defun yank-pop-forward (&optional n)
  "Yank pop by forward direction.
The N-th next kill is inserted.
If called with no argument, insert the next kill"
  (interactive "p")
  (unless n (setq n 1))
  (yank-pop (- n)))

(defun next-line-scroll-up (&optional n)
  "Move cursor to next screen line and scroll up.
Increment is N. If N is ommited, use 1."
  (interactive "p")
  (unless n (setq n 1))
  (vertical-motion n)
  (scroll-up n))

(defun previous-line-scroll-down (n)
  "Move cursor to previous screen line and scroll down.
Increment is N. If N is ommited, use 1."
  (interactive "p")
  (unless n (setq n 1))
  (vertical-motion (- n))
  (scroll-down n))

(defun search-forward-char (&optional char)
  (interactive)
  (let ((c (cond ((characterp char) char)
                 ((stringp char) (string-to-char char))
                 ((eq this-command last-command) (following-char))
                 (t (read-char "char: ")))))
    (if (and (characterp c)
             (search-forward (char-to-string c) (point-at-eol) t
                        (if (eq c (following-char) ) 2 1)))
        (backward-char)
      (setq this-command last-command))))

(defun search-backward-char (&optional char)
  (interactive)
  (let ((c (cond ((characterp char) char)
                 ((stringp char) (string-to-char char))
                 ((eq this-command last-command) (following-char))
                 (t (read-char "char: ")))))
    (unless (and (characterp c)
                 (search-backward (char-to-string c) (point-at-bol) t))
      (setq this-command last-command))))


(resolve functions)
