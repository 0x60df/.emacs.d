
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


(resolve font)
