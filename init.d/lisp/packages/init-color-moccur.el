
;;;; init-color-moccur.el



;;; base

(premise init)
(premise inst-color-moccur)

(autoload 'moccur "color-moccur"
    "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *Moccur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how." t)
(setq moccur-split-word t)


;;; bindings

(global-set-key "\C-csm" 'moccur)


(resolve init-color-moccur)
