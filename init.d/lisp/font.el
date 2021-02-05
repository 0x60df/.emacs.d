
;;;; font.el

(premise init)

(defun set-frame-fontset (fontset &optional frame)
  "Set FRAME font by FONTSET."
  (interactive (list (completing-read "Fontset:"
                                      (mapcar
                                       (lambda (name)
                                         (replace-regexp-in-string
                                          ".+fontset-\\(.+?\\)$" "\\1"
                                          name))
                                       (fontset-list)))))
  (modify-frame-parameters frame `((font . ,(concat "fontset-" fontset)))))

(defvar switch-frame-fontset-list nil
  "List of fontset for switching current frame fontset.")
(make-variable-buffer-local 'switch-frame-fontset-list)

(defun switch-frame-fontset (&optional n)
  "Switch frame fontset among `switch-frame-fontset-list'."
  (interactive "p")
  (if switch-frame-fontset-list
      (let* ((fontset (frame-parameter nil 'font-parameter))
             (rest (seq-drop-while
                    (lambda (f) (not (equal f fontset)))
                    switch-frame-fontset-list))
             (length (length switch-frame-fontset-list))
             (offset (- length (length rest))))
        (set-frame-parameter
         nil 'font
         (nth (if rest (% (+ (% (+ n offset) length) length) length) 1)
              switch-frame-fontset-list)))))


(resolve font)
