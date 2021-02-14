
;;;; init-which-key.el


(premise init)
(premise custom)
(premise inst-which-key)

(declare-function which-key--propertize "which-key")

(custom-set-variables
 '(which-key-idle-delay 0.4)
 '(which-key-lighter ""))

(defun patch-which-key--next-page-hint (return)
  "Advising `which-key--next-page-hint' to modify hint text.
Replace `key-description' of `help-char' by
`text-char-description' of `help-char' suffixed with /?,
e.g. C-h by ^H/?."
  (if (stringp return)
      (replace-regexp-in-string
       (key-description (char-to-string help-char))
       (which-key--propertize (concat (text-char-description help-char) "/?")
                              'face 'which-key-note-face)
       return)
    return))

(with-eval-after-load 'which-key
  (advice-add 'which-key--next-page-hint
              :filter-return #'patch-which-key--next-page-hint))

(add-hook 'emacs-startup-hook #'which-key-mode)


(resolve init-which-key)
