
;;;; mode-line.el


(premise init)
(premise custom)
(premise simple)
(premise client)
(premise feature)

(lazy-autoload 'seq-difference "seq")

(require 'which-func)


;;; faces

(face-spec-set 'mode-line-highlight
               '((t (:inverse-video t)))
               'face-defface-spec)

(face-spec-set 'mode-line-emphasis
               '((t (:weight bold :slant italic)))
               'face-defface-spec)

(face-spec-set 'mode-line-buffer-id
               '((t (:weight bold)))
               'face-defface-spec)

(face-spec-set 'which-func
               '((t (:slant italic)))
               'face-defface-spec)

(face-spec-set 'vc-state-base
               '((t :slant italic))
               'face-defface-spec)

(defface mode-line-mode-name
  '((t :weight bold))
  "Face used for mode-name part of the mode line."
  :group 'user)

(defface mode-line-warning
  '((t :weight bold :slant italic :inverse-video t))
  "Face for the potion of mode-line which call attention."
  :group 'user)

(defface mode-line-transform
  '((t :underline t))
  "Face for the potion of mode-line which is transformed."
  :group 'user)

(defface mode-line-separator
  '((t))
  "Face for separator space of the mode line."
  :group 'user)



;;; format

(setq mode-line-front-space
      '(:eval
        (if (display-graphic-p)
            (propertize " "
                        'display
                        '(space :align-to (+ left-margin left-fringe)))
          "")))

(define-minor-mode mode-line-mule-info-showing-input-method-mode
  "Minor to show input method in mode line construct for mule info."
  :group 'user)

(setq-default mode-line-mule-info
              '((mode-line-mule-info-showing-input-method-mode
                 (current-input-method current-input-method-title))
                "%Z"))

(setq mode-line-client
      '(:eval
        (let ((client (frame-parameter nil 'client)))
          (if client
              (let ((p (1+ (seq-position (reverse server-clients)
                                         client #'eq))))
                (if (< p 10) (number-to-string p) "#"))
            ""))))

(setq-default mode-line-modified '("%1*%1+"))

(setq-default mode-line-remote '("%1@"))

(setq mode-line-frame-identification
      '((:eval
         (let ((p (1+ (seq-position (reverse
                                     (if (frame-parameter nil 'client)
                                         (client-frame-list)
                                       (frame-list)))
                                    (selected-frame) #'eq))))
           (if (< p 10) (number-to-string p) "#")))
        (:propertize " " face mode-line-separator)))

(define-minor-mode mode-line-buffer-identification-shrink-mode
  "Minor mode to shrink mode line construct for buffer identification."
  :group 'user)

(setq-default mode-line-buffer-identification
              (let* ((max-width 12)
                     (format (format "%%%db" max-width)))
                `(mode-line-buffer-identification-shrink-mode
                  (:eval
                   (let* ((text (format-mode-line ,format))
                          (canonicalized
                           (replace-regexp-in-string
                            "%" "%%"
                            (truncate-string-to-width text ,max-width))))
                     (propertize
                      canonicalized
                      'face (if (< ,max-width (string-width text))
                                '(mode-line-buffer-id
                                  mode-line-transform)
                              'mode-line-buffer-id))))
                  (:propertize ,format face mode-line-buffer-id))))

(custom-set-variables
 '(mode-line-percent-position '(-3 "%p"))
 '(column-number-indicator-zero-based nil))

(setq mode-line-position
      '(""
        mode-line-percent-position
        (size-indication-mode (5 "/%I"))
        (line-number-mode
         (column-number-mode
          (column-number-indicator-zero-based
           (7  " %l %c")
           (7  " %l %C"))
          (4  " L%l"))
         (column-number-mode
          (column-number-indicator-zero-based
           (4  " C%c")
           (4  " C%C"))))))

(defvar-local mode-line-vc-mode-shrink-width 1
  "Width of shrinking mode line construct for vc mode.
Most configuration which sets this variable and derives
local variable should take care that local variable after
realated operation.")

(define-minor-mode mode-line-vc-mode-shrink-mode
  "Minor mode to shrink mode line construct for vc mode."
  :group 'user)

(setcdr (assq 'vc-mode mode-line-format)
        '(((:propertize " " face mode-line-separator)
           (:eval (let ((string (replace-regexp-in-string
                                 "^\\s-+\\|\\s-+$" "" vc-mode)))
                    (remove-list-of-text-properties
                     0 (length string) '(mouse-face local-map help-echo) string)
                    (if mode-line-vc-mode-shrink-mode
                        (let* ((max-width mode-line-vc-mode-shrink-width)
                               (canonicalized
                                (replace-regexp-in-string
                                 "%" "%%"
                                 (if (< max-width (string-width string))
                                     (let ((truncated (truncate-string-to-width
                                                       string max-width)))
                                       (if (string-suffix-p " " truncated)
                                           (substring truncated
                                                      0
                                                      (- (length truncated) 1))
                                         truncated))
                                   string))))
                          (when (< max-width (string-width string))
                            (setq string canonicalized)
                            (add-face-text-property
                             0 (length string) 'mode-line-transform t string))))
                    string)))))

(defvar-local mode-line-mode-name-shrink-width 1
  "Width of shrinking mode line construct for mode name.
Most configuration which sets this variable and derives
local variable should take care that local variable after
realated operation.")

(define-minor-mode mode-line-mode-name-shrink-mode
  "Minor mode to shrink mode line construct for mode name."
  :group 'user)

(defvar-local mode-line-minor-mode-shrink-width 12
  "Width of shrinking mode line construct for minor mode.
Most configuration which sets this variable and derives
local variable should take care that local variable after
realated operation.")

(define-minor-mode mode-line-minor-mode-shrink-mode
  "Minor mode to shrink mode line construct for minor mode."
  :group 'user)

(setq mode-line-modes
      '("%["
        (mode-line-mode-name-shrink-mode
         (:eval
          (let* ((max-width mode-line-mode-name-shrink-width)
                 (text (format-mode-line mode-name))
                 (canonicalized
                  (replace-regexp-in-string
                   "%" "%%"
                   (if (< max-width (string-width text))
                       (let ((truncated (truncate-string-to-width
                                         text max-width)))
                         (if (string-suffix-p " " truncated)
                             (substring truncated 0 (- (length truncated) 1))
                           truncated))
                     text))))
            (propertize canonicalized
                        'face (if (< max-width (string-width text))
                                  '(mode-line-mode-name mode-line-transform)
                                'mode-line-mode-name))))
         (:propertize mode-name face mode-line-mode-name))
        mode-line-process
        (mode-line-minor-mode-shrink-mode
         (:eval
          (let* ((max-width mode-line-minor-mode-shrink-width)
                 (text (format-mode-line minor-mode-alist))
                 (canonicalized
                  (replace-regexp-in-string
                   "%" "%%"
                   (if (< max-width (string-width text))
                       (let ((truncated (truncate-string-to-width
                                         text max-width)))
                         (if (string-suffix-p " " truncated)
                             (substring truncated 0 (- (length truncated) 1))
                           truncated))
                     text))))
            (if (< max-width (string-width text))
                (add-face-text-property
                 0 (length canonicalized) 'mode-line-transform t canonicalized))
            canonicalized))
         minor-mode-alist)
        (:propertize (-4 "%n") face mode-line-emphasis)
        "%]"
        (:propertize " " face mode-line-separator)))

(defcustom mode-line-minor-mode-priority-alist nil
  "Alist of minor mode and its priority for mode line display.
Each element looks like (mode-variable . priority)
0:      Input method
1--9:   Associated with specific major mode.
10--19: Almost always enabled.
20--29: Transiently enabled.
30--39: Occasionally enabled.
40--49: Almost manually enabled."
  :type '(repeat sexp)
  :group 'user)

(push '(auto-overwrite-mode . 40) mode-line-minor-mode-priority-alist)
(push '(auto-fill-function . 49) mode-line-minor-mode-priority-alist)

(defun mode-line-sort-alist (alist-by-symbol priority-alist)
  "Return sorted ALIST-BY-SYMBOL according to PRIORITY-ALIST.

Keys of ALIST-BY-SYMBOL must be symbol.
PRIORITY-ALIST looks like
(key-for-ALIST-BY-SYMBOL . priority).

ALIST-BY-SYMBOL is sorted destructively.

Cells are compared by specified priorities.
If some entries have same priority, they are compared by
`symbol-name'. Entries to which no pritority is spceified are
regarded as having lower priority than specified one.
They also compared by `symbol-name'."
  (sort alist-by-symbol
        (lambda (cell1 cell2)
          (let* ((symbol1 (car cell1))
                 (symbol2 (car cell2))
                 (priority1 (cdr (assq symbol1 priority-alist)))
                 (priority2 (cdr (assq symbol2 priority-alist)))
                 (name1 (symbol-name symbol1))
                 (name2 (symbol-name symbol2)))
            (cond ((and priority1 priority2)
                   (if (eql priority1 priority2)
                       (string-lessp name1 name2)
                     (< priority1 priority2)))
                  (priority1 t)
                  (priority2 nil)
                  (t (string-lessp name1 name2)))))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq minor-mode-alist (mode-line-sort-alist
                                    (copy-tree minor-mode-alist)
                                    mode-line-minor-mode-priority-alist))
            (add-variable-watcher
             'minor-mode-alist
             (lambda (symbol newval operation where)
               (when (consp newval)
                 (let ((sorted (mode-line-sort-alist
                                (copy-tree newval)
                                mode-line-minor-mode-priority-alist)))
                   (setcar newval (car sorted))
                   (setcdr newval (cdr sorted))))))))

(custom-set-variables
 '(which-func-format
   '(:propertize which-func-current face which-func)))

(setq mode-line-misc-info
      '((which-function-mode
         (which-func-mode
          ("" which-func-format " ")))
        (global-mode-string ("" global-mode-string " "))))

(setq mode-line-end-spaces "")

(defcustom mode-line-boundary-faces
  '(mode-line-buffer-id
    which-func
    vc-base
    vc-conflict-state
    vc-edited-state
    vc-locally-added-state
    vc-locked-state
    vc-missing-state
    vc-needs-update-state
    vc-removed-state
    vc-state-base
    vc-up-to-date-state
    mode-line-mode-name
    mode-line-separator)
  "Faces which determine boundary of elements of mode-line."
  :type '(repeat face)
  :group 'user)

(defun mode-line--face-list (position object)
  "Return list of face or face spec on the POSITION of the OBJECT."
  (let ((face (get-text-property position 'face object)))
    (cond ((symbolp face) (list face))
          ((stringp face) (list (intern face)))
          ((eq (car face) :filtered) (list face))
          ((keywordp (car face)) (list face))
          ((memq (car face) '(foreground-color background-color)) (list face))
          ((listp face) face)
          (t '()))))

(defun mode-line--next-boundary (position object limit)
  "Return next bounday of mode line elments by integer.
Search `next-single-property-change' for the face from
POSITION in OBJECT. If difference contains
`mode-line-boundary-faces', return that position.
If no boundary is detected until LIMIT, retun nil."
  (let ((candidate (next-single-property-change position 'face object limit)))
    (cond ((eql candidate limit) candidate)
          ((let* ((face-1 (mode-line--face-list candidate object))
                  (face-2 (mode-line--face-list (- candidate 1) object))
                  (diff-1 (seq-difference face-1 face-2))
                  (diff-2 (seq-difference face-2 face-1)))
             (seq-some (lambda (face) (member face mode-line-boundary-faces))
                       (append diff-1 diff-2)))
           candidate)
          (t (mode-line--next-boundary candidate object limit)))))

(defun mode-line--previous-boundary (position object limit)
  "Return previous bounday of mode line elments by integer.
Search `previous-single-property-change' for the face from
POSITION in OBJECT. If difference contains
`mode-line-boundary-faces', return that position.
If no boundary is detected until LIMIT, retun nil."
  (let ((candidate (previous-single-property-change position 'face
                                                    object limit)))
    (cond ((eql candidate limit) candidate)
          ((let* ((face-1 (mode-line--face-list candidate object))
                  (face-2 (mode-line--face-list (- candidate 1) object))
                  (diff-1 (seq-difference face-1 face-2))
                  (diff-2 (seq-difference face-2 face-1)))
             (seq-some (lambda (face) (member face mode-line-boundary-faces))
                       (append diff-1 diff-2)))
           candidate)
          (t (mode-line--previous-boundary (- candidate 1) object limit)))))

(defun mode-line--duplicate-percent-with-text-properties (string)
  "Duplicate percent in STRING with text properties."
  (letrec ((duplicate-next-percent
            (lambda (index)
              (let ((next (string-match "%" string index)))
                (cond ((null next) (substring string index (length string)))
                      (t (concat
                          (substring string index next)
                          (substring string next (+ next 1))
                          (substring string next (+ next 1))
                          (funcall duplicate-next-percent (+ next 1)))))))))
    (funcall duplicate-next-percent 0)))

(define-minor-mode mode-line-format-auto-truncate-on-boundary-mode
  "Minor mode to truncate on boundary of mode line elements."
  :group 'user)

(defvar mode-line-format-raw
  (mapcar (lambda (e)
            (if (stringp e)
                (let ((string (replace-regexp-in-string "^\\s-+$" " " e)))
                  (if (equal string " ")
                      (propertize string 'face 'mode-line-separator)
                    string))
              e))
          (default-value 'mode-line-format))
  "Default value of `mode-line-format'.")
(make-variable-buffer-local 'mode-line-format-raw)

(defun mode-line-format-auto-truncate (form)
  "Construct form for `mode-line-format' from FORM.
Constructed form wrap FORM by :eval form which truncate
mode-line string by window-width."
  `(:eval
    (let* ((text (format-mode-line ',form))
           (text-width (string-width text))
           (max-width (+ (window-body-width) 1))
           (shrunk
            (if (< max-width text-width)
                (let* ((subtext (truncate-string-to-width text max-width))
                       (length (length subtext))
                       (r-subtext (string-reverse subtext))
                       (last-non-space
                        (- length 1 (string-match "[^ ]" r-subtext)))
                       (last-element-end
                        (mode-line--next-boundary
                         last-non-space subtext length))
                       (original-last-element-end
                        (mode-line--next-boundary
                         last-non-space text (length text))))
                  (if (and (< last-element-end original-last-element-end)
                           (string-match
                            "[^ ]"
                            (substring text
                                       last-element-end
                                       original-last-element-end)))
                      (let ((last-element-start
                             (mode-line--previous-boundary
                              (+ last-non-space 1) subtext 0)))
                        (if mode-line-format-auto-truncate-on-boundary-mode
                            (setq subtext (substring subtext 0
                                                     last-element-start))
                          (let ((property (get-text-property
                                           last-non-space 'face subtext)))
                            (add-face-text-property
                             last-element-start length 'mode-line-transform
                             nil subtext)))))
                  subtext)
              text)))
      (mode-line--duplicate-percent-with-text-properties shrunk))))

(defun mode-line-show-truncated ()
  "Show truncated section of mode-line in echo area."
  (interactive)
  (let* ((text (format-mode-line mode-line-format-raw))
         (text-width (string-width text))
         (max-width (+ (window-body-width) 1)))
    (when (< max-width text-width)
      (let* ((subtext (truncate-string-to-width
                       text
                       (if mode-line-format-auto-truncate-on-boundary-mode
                           (mode-line--previous-boundary max-width text 0)
                         max-width)))
             (truncated (replace-regexp-in-string
                         "\\s-+$" ""
                         (substring text (length subtext) (length text))))
             (width (length truncated))
             (first-element-end (mode-line--next-boundary 0 truncated width)))
        (unless mode-line-format-auto-truncate-on-boundary-mode
          (add-face-text-property 0 first-element-end 'mode-line-transform
                                  nil truncated))
        (let ((message-log-max nil))
          (message "%s" truncated))))))

(defun mode-line-set-showing-timer ()
  "Set timer for `mode-line-show-truncated'"
  (run-with-idle-timer 0.8 nil (lambda ()
                                 (unless (current-message)
                                   (mode-line-show-truncated)))))

(define-minor-mode mode-line-auto-show-truncated-mode
  "Minor mode to show truncated section automatically."
  :group 'user
  (if mode-line-auto-show-truncated-mode
      (add-hook 'post-command-hook #'mode-line-set-showing-timer nil t)
    (remove-hook 'post-command-hook #'mode-line-set-showing-timer t)))

(define-globalized-minor-mode mode-line-auto-show-truncated-global-mode
  mode-line-auto-show-truncated-mode mode-line-auto-show-truncated-mode
  :group 'user)

(add-hook 'mode-line-auto-show-truncated-mode-hook
          (lambda ()
            (if mode-line-auto-show-truncated-mode
                (mode-line-show-truncated))))

(setq-default mode-line-format
              (mode-line-format-auto-truncate mode-line-format-raw))

(add-hook 'emacs-startup-hook #'mode-line-auto-show-truncated-global-mode)



;;; settings

(which-function-mode)
(column-number-mode)
(line-number-mode)

(custom-set-variables
 '(eol-mnemonic-dos "+")
 '(eol-mnemonic-mac "!")
 '(eol-mnemonic-unix ":"))

(add-hook 'emacs-startup-hook #'mode-line-auto-show-truncated-mode)



;;; utilities

(defun enhance-mode-line-format (function &rest args)
  "Around advice to make `mode-line-format' enhanced.
Specifically, add front space and setup auto truncate.
This advice only can work with simple function which does
not modify inner structure of `mode-line-format' but just
set `mode-line-format'"
  (setq mode-line-format mode-line-format-raw)
  (apply function args)
  (setq mode-line-format-raw mode-line-format)
  (setq mode-line-format
        (mode-line-format-auto-truncate
         (list mode-line-front-space mode-line-format-raw))))

(defun modify-minor-mode-lighter (mode-variable lighter)
  "Modify minor mode lighter embedded in `minor-mode-alist'.
Query entry by MODE-VARIABLE and set LIGHTER."
  (let* ((entry (assq mode-variable minor-mode-alist))
         (cdr (cdr entry)))
    (if entry
        (if cdr
            (setcar cdr lighter)
          (setcdr entry `(,lighter))))))

(defun show-which-function ()
  "Show which-function in echo area."
  (interactive)
  (message "Function %s" (which-function)))


(resolve mode-line)
