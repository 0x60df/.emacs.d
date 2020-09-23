
;;;; functions.el


(premise init)

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

(defun next-line-scroll-up (n)
  (interactive "p")
  (forward-line n)
  (scroll-up n))
(defun previous-line-scroll-down (n)
  (interactive "p")
  (forward-line (* -1 n))
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
