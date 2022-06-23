
;;;; font.el

(premise init)
(premise subr)

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



(defcustom startup-font-function
  (lambda ()
    (if (display-graphic-p)
        (create-fontset-from-ascii-font
         "DejaVu Sans Mono-11:weight=normal:slant=normal" nil "user")))
  "Function which returns name of fontset, which is set at startup.
This function takes no argument and should return string.
If return value is not a string valid as fontset fontset
name, no font is initialized at startup.
Because this function is called after initialization,
modification on this variable during loaded init files takes
effect."
  :group 'user
  :type 'function)

(let ((curve (lambda (&optional _)
               (if (functionp startup-font-function)
                   (let ((fontset (funcall startup-font-function)))
                     (when (and (stringp fontset) (fontset-name-p fontset))
                       (add-to-list 'default-frame-alist `(font . ,fontset))
                       (modify-frame-parameters nil `((font . ,fontset)))))))))
  (if (daemonp)
      (add-hook-for-once 'after-make-terminal-functions curve)
    (add-hook 'emacs-startup-hook curve)))


(resolve font)
