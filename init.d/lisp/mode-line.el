
;;;; mode-line.el


(premise init)
(premise custom)
(premise client)

(require 'which-func)


;;; faces

(defface mode-line-buffer-identification-face
  '((t :weight bold))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces)

(defface mode-line-vc-mode-face
  '((t :slant italic))
  "Face used for vc-mode parts of the mode line."
  :group 'mode-line-faces)

(defface mode-line-mode-name-face
  '((t :weight bold))
  "Face used for mode-name parts of the mode line."
  :group 'mode-line-faces)

(defface mode-line-which-func-mode-face
  '((t :slant italic))
  "Face used for which-func-mode parts of the mode line."
  :group 'mode-line-faces)

(defface mode-line-shrinked
  '((t :slant italic))
  "Face for the potion of mode-line which is shirinked."
  :group 'user)


;;; format

(setq-default mode-line-front-space
              '(:eval (if (display-graphic-p)
                          (propertize " "
                                      'display
                                      '(space :align-to
                                              (+ left-margin left-fringe)))
                        "-")))

(defvar mode-line-mule-info-showing-input-method nil
  "State of `mode-line-mule-info' replesentation.
When non-nil, `mode-line-mule-info' shows input method.")
(make-variable-buffer-local 'mode-line-mule-info-showing-input-method)

(setq-default mode-line-mule-info
              '((mode-line-mule-info-showing-input-method
                 (current-input-method current-input-method-title))
                "%Z"))

(defun mode-line-mule-info-toggle-showing-input-method ()
  "Toggle `mode-line-mule-info' showing input method state."
  (interactive)
  (if mode-line-mule-info-showing-input-method
      (setq mode-line-mule-info-showing-input-method nil)
    (setq mode-line-mule-info-showing-input-method t)))

(setq mode-line-client '(:eval (if (frame-parameter nil 'client)
                                   (let ((l (length server-clients)))
                                     (if (< l 10)
                                         (number-to-string l)
                                       "#"))
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

(defvar mode-line-buffer-identification-shrinked nil
  "Status in which `mode-line-buffer-identification' is shrinked or not.")
(make-variable-buffer-local 'mode-line-buffer-identification-shrinked)

(setq-default mode-line-buffer-identification
              (let* ((max-width 12)
                     (format (format "%%%db" max-width)))
                `(mode-line-buffer-identification-shrinked
                  (:eval
                   (let* ((text (format-mode-line ,format))
                          (canonicalized
                           (replace-regexp-in-string
                            "%" "%%" (substring text 0 ,max-width))))
                     (propertize
                      canonicalized
                      'face (if (< ,max-width (length text))
                                '(mode-line-buffer-identification-face
                                  mode-line-shrinked)
                              'mode-line-buffer-identification-face))))
                  (:propertize ,format
                               face mode-line-buffer-identification-face))))

(defun mode-line-buffer-identification-toggle-shrinked ()
  "toggle mode-line-buffer-identification shrink status"
  (interactive)
  (if mode-line-buffer-identification-shrinked
      (setq mode-line-buffer-identification-shrinked nil)
    (setq mode-line-buffer-identification-shrinked t)))

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
            face mode-line-vc-mode-face))))

(defvar mode-line-modes-shrinked nil
  "Status in which `mode-line-modes' is shrinked or not.")
(make-variable-buffer-local 'mode-line-modes-shrinked)

(defun mode-line-modes-toggle-shrinked ()
  "toggle mode-line-modes shrink status"
  (interactive)
  (if mode-line-modes-shrinked
      (setq mode-line-modes-shrinked nil)
    (setq mode-line-modes-shrinked t)))

(setq mode-line-modes
      '("%["
        ((:propertize mode-name face mode-line-mode-name-face)
         mode-line-process
         (mode-line-modes-shrinked
          (:eval
           (let* ((minor-modes (format-mode-line minor-mode-alist))
                  (length (length minor-modes))
                  (max-width 12))
             (if (< max-width length)
                 (propertize
                  (substring minor-modes 0
                             (if (= (aref minor-modes (- max-width 1)) 32)
                                 (- max-width 1)
                               max-width))
                  'face 'mode-line-shrinked)
               minor-modes)))
          minor-mode-alist)
         "%n")
        "%]"
        " "))

(setq mode-line-misc-info
      '((which-func-mode
         (""
          (:propertize which-func-current face mode-line-which-func-mode-face)
          " "))
        (global-mode-string ("" global-mode-string " "))))

(setq-default
 mode-line-format
 `(:eval
   (let* ((text (format-mode-line
                 (quote
                  ,(mapcar (lambda (e)
                        (if (stringp e)
                            (replace-regexp-in-string "^\\s-+$" " " e)
                          e))
                      (default-value 'mode-line-format)))))
          (text-width (length text))
          (window-width (window-body-width)))
     (replace-regexp-in-string
      "%" "%%"
      (if (< text-width (+ window-width 1))
          text
        (let* ((subtext (substring text 0 (+ window-width 1)))
               (length (length subtext))
               (r-text (string-reverse subtext))
               (index (- length 1 (string-match "[^ ]" r-text)))
               (prop (get-text-property index 'face subtext))
               (start (previous-single-property-change
                       index 'face subtext 0))
               (end (next-single-property-change
                     index 'face subtext length))
               (boundary (next-single-property-change
                          index 'face text text-width)))
          (if (and (< end boundary)
                   (string-match "[^ ]" (substring text end boundary)))
              (progn
                (add-text-properties
                 start length
                 (list 'face (if (atom prop)
                                 (list 'mode-line-shrinked prop)
                               (cons 'mode-line-shrinked prop)))
                 subtext)
                subtext)
            subtext)))))))


;;; option setting

(which-function-mode 1)
(column-number-mode t)
(line-number-mode t)


;;; eol

(custom-set-variables
 '(eol-mnemonic-dos "+")
 '(eol-mnemonic-mac "!")
 '(eol-mnemonic-unix ":"))


;;; functions

(defun show-which-function ()
  "Show which-function in echo area."
  (interactive)
  (message "Function %s" (which-function)))


;;; bindings

(global-set-key (kbd "C-c l m") #'mode-line-modes-toggle-shrinked)
(global-set-key (kbd "C-c l b")
                #'mode-line-buffer-identification-toggle-shrinked)
(global-set-key (kbd "C-c l i")
                #'mode-line-mule-info-toggle-showing-input-method)
(global-set-key (kbd "C-c l f") #'show-which-function)


(resolve mode-line)
