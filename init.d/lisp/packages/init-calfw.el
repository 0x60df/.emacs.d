
;;;; init-calfw.el


(premise init)
(premise custom)
(premise subr)
(premise advice)
(premise frame)
(premise inst-calfw)

(eval-when-compile (require 'calfw))

(declare-function cfw:open-calendar-buffer "calfw")
(declare-function cfw:refresh-calendar-buffer "calfw")

(defcustom calfw-contents-sources nil "Contents sources to be used by `calfw'."
  :type '(repeat sexp)
  :group 'user)

(defcustom calfw-annotation-sources nil
  "Annotation sources to be used by `calfw'."
  :type '(repeat sexp)
  :group 'user)

(defcustom calfw-frame-alists nil "List of frame alist for `calfw' frame."
  :type '(repeat (alist :key-type 'symbol :value-type 'sexp))
  :group 'user)

(defcustom calfw-org-source-period-bgcolor nil
  "Period bgcolor for cfw:org-source.
If non-nil, override slot by this value."
  :type 'string
  :group 'user)

(defcustom calfw-org-source-period-fgcolor nil
  "Period fgcolor for cfw:org-source.
If non-nil, override slot by this value."
  :type 'string
  :group 'user)

(defun cfw:org-create-source-set-period-fg-bg (return)
  "Advising `cfw:org-create-source' to set prediod fg and bg."
  (setf (cfw:source-period-bgcolor return) calfw-org-source-period-bgcolor)
  (setf (cfw:source-period-fgcolor return) calfw-org-source-period-fgcolor)
  return)

(advice-add 'cfw:org-create-source
            :filter-return #'cfw:org-create-source-set-period-fg-bg)

(defun calfw ()
  "`cfw:open-calendar-buffer' with default preset."
  (interactive)
  (modify-frame-parameters nil (car calfw-frame-alists))
  (cfw:open-calendar-buffer :contents-sources calfw-contents-sources
                            :annotation-sources calfw-annotation-sources))

(advice-add-for-once 'calfw :before (lambda (&rest args) (require 'calfw)))

(with-eval-after-load 'calfw
  (require 'calfw-org)

  (add-hook 'cfw:calendar-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))

  (define-key cfw:calendar-mode-map ";" #'switch-frame-alist)

  (when (init-unit-p bindings)
    (define-key cfw:calendar-mode-map (kbd "<home>") nil)
    (define-key cfw:calendar-mode-map (kbd "<end>") nil)))


(resolve init-calfw)
