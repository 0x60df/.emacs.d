
;;;; mode-line.el


(premise init)
(premise custom)
(premise client)

(require 'which-func)


;;; faces

(defface mode-line-buffer-identification
  '((t :weight bold))
  "Face used for buffer identification part of the mode line."
  :group 'user)

(defface mode-line-vc-mode
  '((t :slant italic))
  "Face used for vc-mode part of the mode line."
  :group 'user)

(defface mode-line-mode-name
  '((t :weight bold))
  "Face used for mode-name part of the mode line."
  :group 'user)

(defface mode-line-which-func-mode
  '((t :slant italic))
  "Face used for which-func-mode part of the mode line."
  :group 'user)

(defface mode-line-shrinked
  '((t :slant italic))
  "Face for the potion of mode-line which is shirinked."
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
        " "))

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
                                '(mode-line-buffer-identification
                                  mode-line-shrinked)
                              'mode-line-buffer-identification))))
                  (:propertize ,format
                               face mode-line-buffer-identification))))

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
        '((" "
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
                  (propertize canonicalized 'face 'mode-line-shrinked)
                canonicalized)))
           minor-mode-alist)
          (-4 "%n")
          "%]"
          " ")))

(custom-set-variables
 '(which-func-format
   '(:propertize which-func-current face mode-line-which-func-mode)))

(setq mode-line-misc-info
      '((which-function-mode
         (which-func-mode
          ("" which-func-format " ")))
        (global-mode-string ("" global-mode-string " "))))

(setq mode-line-end-spaces
      '(:eval (unless (display-graphic-p) "-%-")))

(setq-default
 mode-line-format
 `(:eval
   (let* ((text
           (format-mode-line
            ',(mapcar (lambda (e)
                        (if (stringp e)
                            (replace-regexp-in-string "^\\s-+$" " " e)
                          e))
                      (default-value 'mode-line-format))))
          (text-width (length text))
          (max-width (+ (window-body-width) 1))
          (shrinked
           (if (< max-width text-width)
               (let* ((subtext (substring text 0 max-width))
                      (r-subtext (string-reverse subtext))
                      (last-non-space
                       (- max-width 1 (string-match "[^ ]" r-subtext)))
                      (last-element-end
                       (next-single-property-change
                        last-non-space 'face subtext max-width))
                      (original-last-element-end
                       (next-single-property-change
                        last-non-space 'face text text-width)))
                 (if (and (< last-element-end original-last-element-end)
                          (string-match
                           "[^ ]"
                           (substring text
                                      last-element-end
                                      original-last-element-end)))
                     (let ((last-element-start
                            (previous-single-property-change
                             (+ last-non-space 1) 'face subtext 0))
                           (property (get-text-property
                                  last-non-space 'face subtext)))
                       (add-text-properties
                        last-element-start max-width
                        (list 'face (if (atom property)
                                        (list 'mode-line-shrinked property)
                                      (cons 'mode-line-shrinked property)))
                        subtext)))
                 subtext)
             text)))
     (letrec ((duplicate-percent
               (lambda (string start)
                 (let ((end (string-match "%" string start)))
                   (cond ((null end) (substring string start (length string)))
                         (t (concat
                             (substring string start end)
                             (substring string end (+ end 1))
                             (substring string end (+ end 1))
                             (funcall duplicate-percent string (+ end 1)))))))))
       (funcall duplicate-percent shrinked 0)))))



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
