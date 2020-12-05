
;;;; mode-line.el


(premise init)
(premise custom)
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

(defface mode-line-vc-mode
  '((t :slant italic))
  "Face used for vc-mode part of the mode line."
  :group 'user)

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
          "-")))

(defvar mode-line-mule-info-showing-input-method-flag nil
  "State of `mode-line-mule-info' replesentation.
When non-nil, `mode-line-mule-info' shows input method.")
(make-variable-buffer-local 'mode-line-mule-info-showing-input-method-flag)

(defun mode-line-mule-info-toggle-showing-input-method ()
  "Toggle `mode-line-mule-info' showing input method state."
  (interactive)
  (if mode-line-mule-info-showing-input-method-flag
      (setq mode-line-mule-info-showing-input-method-flag nil)
    (setq mode-line-mule-info-showing-input-method-flag t)))

(setq-default mode-line-mule-info
              '((mode-line-mule-info-showing-input-method-flag
                 (current-input-method current-input-method-title))
                "%Z"))

(setq mode-line-client
      '(:eval
        (if (frame-parameter nil 'client)
            (let ((l (length server-clients)))
              (if (< l 10) (number-to-string l) "#"))
          "")))

(setq-default mode-line-modified '("%1*%1+"))

(setq-default mode-line-remote '("%1@"))

(setq mode-line-frame-identification
      '((:eval
         (let ((l (length (if (frame-parameter nil 'client)
                              (client-frame-list)
                            (frame-list)))))
           (if (< l 10) (number-to-string l) "#")))
        (:propertize " " face mode-line-separator)))

(defvar mode-line-buffer-identification-shrinked-flag nil
  "State of `mode-line-buffer-identification' replesentation.
When non-nil, `mode-line-buffer-identification' is shrinked.")
(make-variable-buffer-local 'mode-line-buffer-identification-shrinked-flag)

(defun mode-line-buffer-identification-toggle-shrinked ()
  "Toggle `mode-line-buffer-identification' shrinking state."
  (interactive)
  (if mode-line-buffer-identification-shrinked-flag
      (setq mode-line-buffer-identification-shrinked-flag nil)
    (setq mode-line-buffer-identification-shrinked-flag t)))

(setq-default mode-line-buffer-identification
              (let* ((max-width 12)
                     (format (format "%%%db" max-width)))
                `(mode-line-buffer-identification-shrinked-flag
                  (:eval
                   (let* ((text (format-mode-line ,format))
                          (canonicalized
                           (replace-regexp-in-string
                            "%" "%%" (substring text 0 ,max-width))))
                     (propertize
                      canonicalized
                      'face (if (< ,max-width (length text))
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

(setcdr (assq 'vc-mode mode-line-format)
        '(((:propertize " " face mode-line-separator)
           (:propertize
            (:eval (replace-regexp-in-string
                    "^\\s-+\\|\\s-+$" ""
                    (substring-no-properties vc-mode)))
            face mode-line-vc-mode))))

(defvar mode-line-modes-shrinked-flag nil
  "State of `mode-line-modes' replesentation.
When non-nil, `'mode-line-modes is shrinked.")
(make-variable-buffer-local 'mode-line-modes-shrinked-flag)

(defun mode-line-modes-toggle-shrinked ()
  "Toggle `mode-line-modes' shrinking state"
  (interactive)
  (if mode-line-modes-shrinked-flag
      (setq mode-line-modes-shrinked-flag nil)
    (setq mode-line-modes-shrinked-flag t)))

(setq mode-line-modes
      (let ((max-width 12))
        `("%["
          (:propertize mode-name face mode-line-mode-name)
          mode-line-process
          (mode-line-modes-shrinked-flag
           (:eval
            (let* ((text (format-mode-line minor-mode-alist))
                   (canonicalized
                    (replace-regexp-in-string
                     "%" "%%"
                     (if (< ,max-width (length text))
                         (substring text 0
                                    (if (= (aref text (- ,max-width 1)) 32)
                                        (- ,max-width 1)
                                      ,max-width))
                       text))))
              (if (< ,max-width (length text))
                  (propertize canonicalized 'face 'mode-line-transform)
                canonicalized)))
           minor-mode-alist)
          (:propertize (-4 "%n") face mode-line-emphasis)
          "%]"
          (:propertize " " face mode-line-separator))))

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

(setq mode-line-end-spaces
      '(:eval (unless (display-graphic-p) "-%-")))

(defcustom mode-line-boundary-faces
  '(mode-line-buffer-id
    mode-line-vc-mode
    mode-line-mode-name
    which-func
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
           (text-width (length text))
           (max-width (+ (window-body-width) 1))
           (shrinked
            (if (< max-width text-width)
                (let* ((subtext (substring text 0 max-width))
                       (r-subtext (string-reverse subtext))
                       (last-non-space
                        (- max-width 1 (string-match "[^ ]" r-subtext)))
                       (last-element-end
                        (mode-line--next-boundary
                         last-non-space subtext max-width))
                       (original-last-element-end
                        (mode-line--next-boundary
                         last-non-space text text-width)))
                  (if (and (< last-element-end original-last-element-end)
                           (string-match
                            "[^ ]"
                            (substring text
                                       last-element-end
                                       original-last-element-end)))
                      (let ((last-element-start
                             (mode-line--previous-boundary
                              (+ last-non-space 1) subtext 0))
                            (property (get-text-property
                                       last-non-space 'face subtext)))
                        (add-face-text-property
                         last-element-start max-width 'mode-line-transform
                         nil subtext)))
                  subtext)
              text)))
      (mode-line--duplicate-percent-with-text-properties shrinked))))

(setq-default mode-line-format
              (mode-line-format-auto-truncate mode-line-format-raw))



;;; settings

(which-function-mode)
(column-number-mode)
(line-number-mode)

(custom-set-variables
 '(eol-mnemonic-dos "+")
 '(eol-mnemonic-mac "!")
 '(eol-mnemonic-unix ":"))



;;; utilities

(defun show-which-function ()
  "Show which-function in echo area."
  (interactive)
  (message "Function %s" (which-function)))


(resolve mode-line)
