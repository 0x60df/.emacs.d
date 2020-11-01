
;;;; init-which-key.el


(premise init)
(premise custom)
(premise inst-which-key)

(declare-function which-key--propertize "which-key")

(custom-set-variables
 '(which-key-idle-delay 0.4)
 '(which-key-lighter ""))

(defun patch-which-key--next-page-hint (return)
  "Advising function for `which-key--next-page-hint'.
Replace C-h by ^H/?."
  (replace-regexp-in-string
   "C-h"
   (which-key--propertize "^H/?" 'face 'which-key-note-face)
   return))

(with-eval-after-load 'which-key
  (advice-add 'which-key--next-page-hint
              :filter-return #'patch-which-key--next-page-hint))

(add-hook 'emacs-startup-hook #'which-key-mode)


(resolve init-which-key)
