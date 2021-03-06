
;;;; simple.el


(premise init)

(defun duplicate-and-comment ()
  "Duplicate region and comment out them."
  (interactive)
  (if (region-active-p)
      (let ((p (point))
            (rb (region-beginning))
            (re (region-end)))
        (goto-char rb)
        (kill-ring-save rb re)
        (comment-region rb re)
        (yank)
        (newline-and-indent)
        (goto-char p))))

(defun comment-switch ()
  "Scan region, then uncomment whole comment lines and comment others."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (goto-char end)
          (catch 'done
            (while t
              (save-excursion
                (goto-char beg)
                (comment-forward (- end beg))
                (if (< beg (point))
                    (if (< (point) end)
                        (uncomment-region beg (point))
                      (uncomment-region beg end)
                      (throw 'done t))
                  (let ((med (comment-search-forward end 'noerror))
                        (test (lambda (point)
                                (save-excursion
                                  (beginning-of-line)
                                  (comment-forward)
                                  (< (point) point)))))
                    (while (and med (funcall test med))
                      (setq med (comment-search-forward end 'noerror)))
                    (if med
                        (progn
                          (goto-char med)
                          (comment-region beg med))
                      (comment-region beg end)
                      (throw 'done t))))
                (setq beg (point)))
              (setq end (point))))))))

(defun yank-pop-reverse (&optional n)
  "Yank pop by reverse direction.
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
  (if (eql (line-number-at-pos) (line-number-at-pos (1- (window-end))))
      (progn
        (scroll-up n)
        (vertical-motion n))
    (vertical-motion n)
    (scroll-up n)))

(defun previous-line-scroll-down (&optional n)
  "Move cursor to previous screen line and scroll down.
Increment is N. If N is ommited, use 1."
  (interactive "p")
  (unless n (setq n 1))
  (if (eql (line-number-at-pos) (line-number-at-pos (window-start)))
      (progn
        (scroll-down n)
        (vertical-motion (- n)))
    (vertical-motion (- n))
    (scroll-down n)))

(defvar search-char-in-line-matched nil
  "Matched char by search-char-in-line functions.
`search-forward-char-in-line' and
`search-backward-char-in-line' use this.")

(defun search-forward-char-in-line (char)
  "Search forward character on current line from cursor position.
CHAR is searching character.
If called interactively, this function `read-char' for CHAR.
If CHAR is searched successfully, consecutive call searches
same character."
  (interactive
   (list (cond ((and (eq this-command last-command)
                     search-char-in-line-matched)
                search-char-in-line-matched)
               (t (read-char "Char: ")))))
  (let (success)
    (unwind-protect
        (when (and (characterp char)
                   (search-forward
                    (char-to-string char) (line-end-position) t
                    (if (char-equal char (following-char)) 2 1)))
          (backward-char)
          (setq success t))
      (setq search-char-in-line-matched (if success char nil)))))

(defun search-backward-char-in-line (char)
  "Search backward character on this line from cursor position.
CHAR is searching character.
If called interactively, this function `read-char' for CHAR.
If CHAR is searched successfully, consecutive call searches
same character. "
  (interactive
   (list (cond ((and (eq this-command last-command)
                     search-char-in-line-matched)
                search-char-in-line-matched)
               (t (read-char "Char: ")))))
  (let (success)
    (unwind-protect
        (when (and (characterp char)
                   (search-backward
                    (char-to-string char) (line-beginning-position) t))
          (setq success t))
      (setq search-char-in-line-matched (if success char nil)))))


(resolve simple)
