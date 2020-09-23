;;; user-feature-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "fmmm" "fmmm.el" (0 0 0 0))
;;; Generated autoloads from fmmm.el

(autoload 'fmmm-major-mode-p "fmmm" "\
Non-nil if SYMBOL seems to be major mode.

\(fn SYMBOL)" nil nil)

(autoload 'fmmm-major-mode-list "fmmm" "\
Return list consist of major mode symbol." nil nil)

(autoload 'fmmm-minor-mode-p "fmmm" "\
Non-nil if SYMBOL seems to be minor mode.

\(fn SYMBOL)" nil nil)

(autoload 'fmmm-minor-mode-list "fmmm" "\
Return list consist of minor mode symbol." nil nil)

(autoload 'fmmm-enabled-minor-mode-list "fmmm" "\
Return list consist of enabled minor mode symbol." nil nil)

(autoload 'fmmm-disabled-minor-mode-list "fmmm" "\
Return list consist of disabled minor mode symbol." nil nil)

(defvar fmmm-autoload-collector-mode nil "\
Non-nil if Fmmm-Autoload-Collector mode is enabled.
See the `fmmm-autoload-collector-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fmmm-autoload-collector-mode'.")

(custom-autoload 'fmmm-autoload-collector-mode "fmmm" nil)

(autoload 'fmmm-autoload-collector-mode "fmmm" "\
Minor mode for supporting fmmm autoload collecting system.
When enabled, load `fmmm-cache-file', if
`fmmm-major-mode-on-autoload-list', and
`fmmm-minor-mode-on-autoload-list' are nil.
In addition add hook
`fmmm-update-major-mode-on-autoload-list',
`fmmm-update-minor-mode-on-autoload-list',
and `fmmm-save-cache' to `kill-meacs-hook'

If called interactively, enable Fmmm-Autoload-Collector mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fmmm" '("fmmm-")))

;;;***

;;;### (autoloads nil "scratch" "scratch.el" (0 0 0 0))
;;; Generated autoloads from scratch.el

(autoload 'scratch-mode "scratch" "\
Toggle `scratch-mode'.

If called interactively, enable Scratch mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'scratch "scratch" "\
Generate new buffer instantly." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scratch" '("scratch-")))

;;;***

;;;### (autoloads nil "scratchb" "scratchb.el" (0 0 0 0))
;;; Generated autoloads from scratchb.el

(autoload 'scratchb-flush "scratchb" "\
`erase-buffer' and `set-buffer-modified-p' nil on *scratch* buffer." t nil)

(autoload 'scratchb-revert "scratchb" "\
Generate *scratch* buffer if it does not exist." t nil)

(autoload 'scratchb-snapshot "scratchb" "\
Write *scratch* buffer content to `scratchb-snapshot-directory'" t nil)

(autoload 'scratchb-mode-buffer-sticky "scratchb" "\
Enable `scratchb-mode', and reserve enabling on change of major mode.
Reservation is restricted on current buffer." nil nil)

(defvar scratchb-auto-revert-mode nil "\
Non-nil if Scratchb-Auto-Revert mode is enabled.
See the `scratchb-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `scratchb-auto-revert-mode'.")

(custom-autoload 'scratchb-auto-revert-mode "scratchb" nil)

(autoload 'scratchb-auto-revert-mode "scratchb" "\
Toggle `scratchb-auto-revert-mode'.

If called interactively, enable Scratchb-Auto-Revert mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar scratchb-auto-snapshot-mode nil "\
Non-nil if Scratchb-Auto-Snapshot mode is enabled.
See the `scratchb-auto-snapshot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `scratchb-auto-snapshot-mode'.")

(custom-autoload 'scratchb-auto-snapshot-mode "scratchb" nil)

(autoload 'scratchb-auto-snapshot-mode "scratchb" "\
Toggle `scratchb-auto-snapshot-mode'.

If called interactively, enable Scratchb-Auto-Snapshot mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scratchb" '("scratchb-")))

;;;***

;;;### (autoloads nil "sdired" "sdired.el" (0 0 0 0))
;;; Generated autoloads from sdired.el

(autoload 'sdired-sort "sdired" "\
Interface for sort. Basically toggle key.
 if called with prefix argument, offer other features.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sdired" '("sdired-")))

;;;***

;;;### (autoloads nil "shifter" "shifter.el" (0 0 0 0))
;;; Generated autoloads from shifter.el

(autoload 'shifter-shift-major-mode "shifter" "\
shift major mode" t nil)

(autoload 'shifter-shift-minor-mode "shifter" "\
shift minor mode" t nil)

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

If called interactively, enable Shifter-Non-Volatile-Hist mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shifter" '("shifter-")))

;;;***

(provide 'user-feature-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; user-feature-loaddefs.el ends here
