;;; user-feature-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "fmmm" "fmmm.el" (22846 42667 110899 454000))
;;; Generated autoloads from fmmm.el

(autoload 'fmmm-declare-major-mode "fmmm" "\
Declare major mode for fmmm.

\(fn &rest ARGS)" nil nil)

(autoload 'fmmm-declare-minor-mode "fmmm" "\
Declare minor mode for fmmm.

\(fn &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "scratch" "scratch.el" (22846 39519 316546
;;;;;;  353000))
;;; Generated autoloads from scratch.el

(defvar scratch-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-q") #'scratch-kill-current-buffer) map) "\
Keymap for scratch-mode.")

(autoload 'scratch-kill-current-buffer "scratch" "\
Kill current buffer

\(fn)" t nil)

(autoload 'scratch-mode "scratch" "\
Toggle `scratch-mode'.

\(fn &optional ARG)" t nil)

(autoload 'scratch "scratch" "\
Generate new buffer instantly.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "scratchb" "scratchb.el" (22845 15811 250042
;;;;;;  77000))
;;; Generated autoloads from scratchb.el

(defvar scratchb-snapshot-directory (concat user-emacs-directory "scratchb") "\
Directory in which snapshots of scratch buffer is saved.")

(custom-autoload 'scratchb-snapshot-directory "scratchb" t)

(defvar scratchb-default-directory "~" "\
Directory which is selected when scratch buffer is reverted.")

(custom-autoload 'scratchb-default-directory "scratchb" t)

(defvar scratchb-snapshot-limit 256 "\
Number of limit for scratchb snapshot files.")

(custom-autoload 'scratchb-snapshot-limit "scratchb" t)

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

;;;### (autoloads nil "shifter" "shifter.el" (22849 23431 669205
;;;;;;  421000))
;;; Generated autoloads from shifter.el

(autoload 'shifter-shift-major-mode "shifter" "\
shift major mode

\(fn)" t nil)

(autoload 'shifter-shift-minor-mode "shifter" "\
shift minor mode

\(fn)" t nil)

(autoload 'shifter-turn-on-minor-mode "shifter" "\
turn on minor mode

\(fn FORCE)" t nil)

(autoload 'shifter-turn-off-minor-mode "shifter" "\
turn off minor mode

\(fn FORCE)" t nil)

(defvar shifter-non-volatile-hist-mode nil "\
Non-nil if Shifter-Non-Volatile-Hist mode is enabled.
See the `shifter-non-volatile-hist-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `shifter-non-volatile-hist-mode'.")

(custom-autoload 'shifter-non-volatile-hist-mode "shifter" nil)

(autoload 'shifter-non-volatile-hist-mode "shifter" "\
Minor mode for supporting shifter history system.
When enabled, load `shifter-hist-file' if shifter-major/minor-mode-hist are nil.
When kill emacs, save shifter-major/minor-mode-hist if this mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

(provide 'user-feature-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; user-feature-loaddefs.el ends here
