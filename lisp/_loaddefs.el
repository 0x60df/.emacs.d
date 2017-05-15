;;; _loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "scratchb" "scratchb.el" (22809 45591 923747
;;;;;;  140000))
;;; Generated autoloads from scratchb.el

(defvar scratchb-snapshot-directory (concat user-emacs-directory "scratchb") "\
Directory in which snapshots of scratch buffer is saved.")

(custom-autoload 'scratchb-snapshot-directory "scratchb" t)

(defvar scratchb-before-flush-hook nil "\
Hook run before `scratchb-flush'.")

(defvar scratchb-after-flush-hook nil "\
Hook run after `scratchb-flush'.")

(defvar scratchb-before-revert-hook nil "\
Hook run before `scratchb-revert'.")

(defvar scratchb-after-revert-hook nil "\
Hook run after `scratchb-revert'.")

(autoload 'scratchb-flush "scratchb" "\
`erase-buffer' and `set-buffer-modified-p' nil on *scratch* buffer.

\(fn)" t nil)

(autoload 'scratchb-revert "scratchb" "\
Generate *scratch* buffer if it does not exist.

\(fn)" t nil)

(autoload 'scratchb-snapshot "scratchb" "\
Write *scratch* buffer content to `scratchb-snapshot-directory'

\(fn)" t nil)

(defvar scratchb-mode nil "\
Non-nil if Scratchb mode is enabled.
See the `scratchb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `scratchb-mode'.")

(custom-autoload 'scratchb-mode "scratchb" nil)

(autoload 'scratchb-mode "scratchb" "\
Toggle `scratchb-mode'.

In `scratchb-mode' *scratch*
  - buffer is reverted automatically
  - snapshot of content is taken when quit emacs or flush *scratch* buffer

\(fn &optional ARG)" t nil)

;;;***

(provide '_loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; _loaddefs.el ends here
