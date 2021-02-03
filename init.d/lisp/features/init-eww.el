
;;;; init-eww.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)

(eval-when-compile (require 'eww))

(declare-function eww-enhance-header-line-format load-file-name t t)

(custom-set-variables
 '(eww-download-directory (concat user-emacs-directory "eww")))

(overriding-set-key (kbd "C-c w") #'eww)

(defvar eww-header-line-format-raw ""
  "Raw value of `eww-header-line-format'.")
(make-variable-buffer-local 'eww-header-line-format-raw)

(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook (lambda () (setq show-trailing-whitespace nil)))

  (setq eww-header-line-format-raw eww-header-line-format)

  (defun eww-enhance-header-line-format (function &rest args)
    "Around advice to make `header-line-format' enhanced.
Specifically, add front space and setup auto truncate.
This advice only can work with simple function which does
not modify inner structure of `header-line-format' but just
set `header-line-format'"
    (setq header-line-format eww-header-line-format-raw)
    (apply function args)
    (setq eww-header-line-format-raw header-line-format)
    (setq header-line-format
          (mode-line-format-auto-truncate
           (list mode-line-front-space (if (stringp eww-header-line-format-raw)
                                           (replace-regexp-in-string
                                            "^ +" ""
                                            eww-header-line-format-raw)
                                         eww-header-line-format-raw)))))

  (mapc (lambda (function)
          (advice-add function :around #'eww-enhance-header-line-format))
        '(eww-update-header-line-format
          eww-bookmark-prepare
          eww-list-histories
          eww-list-buffers)))


(resolve init-eww)
