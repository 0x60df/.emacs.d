
;;;; init-ediff.el


(premise init)
(premise custom)
(premise subr)
(premise feature)
(premise mode-line)
(premise bindings)

(lazy-autoload 'ediff-with-current-buffer "ediff-init")

(declare-function ediff-modify-mode-line-format load-file-name t t)

(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-default))

(defvar ediff-departure-frame-depot nil
  "Delivery point for `ediff-departure-frame'")

(defvar ediff-departure-frame nil
  "Temporarily keep frame on which ediff is called.")
(make-variable-buffer-local 'ediff-departure-frame)

(defun ediff-restore-departure-frame ()
  "Restore departure frame for ediff."
  (add-hook-for-once
   'post-command-hook
   (lambda ()
     (when ediff-departure-frame-depot
       (unwind-protect
           (select-frame-set-input-focus ediff-departure-frame-depot)
         (setq ediff-departure-frame-depot nil))))))

(with-eval-after-load 'ediff
  (add-hook 'ediff-before-setup-control-frame-hook
            (lambda ()
              (setq ediff-departure-frame-depot (selected-frame))))
  (add-hook 'ediff-after-setup-control-frame-hook
            (lambda ()
              (setq ediff-departure-frame ediff-departure-frame-depot)
              (setq ediff-departure-frame-depot nil)))

  (add-hook 'ediff-cleanup-hook
            (lambda ()
              (setq ediff-departure-frame-depot ediff-departure-frame)))
  (add-hook 'ediff-quit-hook #'ediff-restore-departure-frame)

  (defconst ediff-mode-line-marker ""
    "Marker for indicating ediff modified `mode-line-format'")

  (defvar ediff-modifying-mode-line-format nil
    "Flag if ediff is modifying `mode-line-format.'")

  (defun ediff-modify-mode-line-format (function &rest args)
    "Advice for ediff functions which modify `mode-line-format'."
    (if ediff-modifying-mode-line-format
        (apply function args)
      (setq ediff-modifying-mode-line-format t)

      (ediff-with-current-buffer ediff-buffer-A
        (if (not (and (consp mode-line-format-raw)
                      (memq 'ediff-mode-line-marker mode-line-format-raw)))
            (setq mode-line-format
                  (remq 'mode-line-front-space mode-line-format-raw))
          (setq mode-line-format
                (cadr (seq-drop-while
                       (lambda (e) (not (eq e 'ediff-mode-line-marker)))
                       mode-line-format-raw)))
          (setcar mode-line-format (concat " " (cadr (car mode-line-format))))
          (setcar (cdr mode-line-format)  (cadr (cadr mode-line-format)))))
      (ediff-with-current-buffer ediff-buffer-B
        (if (not (and (consp mode-line-format-raw)
                      (memq 'ediff-mode-line-marker mode-line-format-raw)))
            (setq mode-line-format
                  (remq 'mode-line-front-space mode-line-format-raw))
          (setq mode-line-format
                (cadr (seq-drop-while
                       (lambda (e) (not (eq e 'ediff-mode-line-marker)))
                       mode-line-format-raw)))
          (setcar mode-line-format (concat " " (cadr (car mode-line-format))))
          (setcar (cdr mode-line-format)  (cadr (cadr mode-line-format)))))
      (if ediff-3way-job
          (ediff-with-current-buffer ediff-buffer-C
            (if (not (and (consp mode-line-format-raw)
                          (memq 'ediff-mode-line-marker mode-line-format-raw)))
                (setq mode-line-format
                      (remq 'mode-line-front-space mode-line-format-raw))
              (setq mode-line-format
                    (cadr (seq-drop-while
                           (lambda (e) (not (eq e 'ediff-mode-line-marker)))
                           mode-line-format-raw)))
              (setcar mode-line-format
                      (concat " " (cadr (car mode-line-format))))
              (setcar (cdr mode-line-format)  (cadr (cadr mode-line-format))))))
      (if (ediff-buffer-live-p ediff-ancestor-buffer)
          (ediff-with-current-buffer ediff-ancestor-buffer
            (if (not (and (consp mode-line-format-raw)
                          (memq 'ediff-mode-line-marker mode-line-format-raw)))
                (setq mode-line-format
                      (remq 'mode-line-front-space mode-line-format-raw))
              (setq mode-line-format
                    (cadr (seq-drop-while
                           (lambda (e) (not (eq e 'ediff-mode-line-marker)))
                           mode-line-format-raw)))
              (setcar mode-line-format
                      (concat " " (cadr (car mode-line-format))))
              (setcar (cdr mode-line-format)  (cadr (cadr mode-line-format))))))

      (apply function args)

      (ediff-with-current-buffer ediff-buffer-A
        (setcar mode-line-format
                `(:propertize ,(replace-regexp-in-string
                                "^ " "" (car mode-line-format))
                              face mode-line-separator))
        (setcar (cdr mode-line-format)
                `(:propertize ,(cadr mode-line-format)
                              face mode-line-separator))
        (setq mode-line-format-raw `("" mode-line-front-space
                                     ediff-mode-line-marker ,mode-line-format))
        (setq mode-line-format
              (mode-line-format-auto-truncate mode-line-format-raw)))
      (ediff-with-current-buffer ediff-buffer-B
        (setcar mode-line-format
                `(:propertize ,(replace-regexp-in-string
                                "^ " "" (car mode-line-format))
                              face mode-line-separator))
        (setcar (cdr mode-line-format)
                `(:propertize ,(cadr mode-line-format)
                              face mode-line-separator))
        (setq mode-line-format-raw `("" mode-line-front-space
                                     ediff-mode-line-marker ,mode-line-format))
        (setq mode-line-format
              (mode-line-format-auto-truncate mode-line-format-raw)))
      (if ediff-3way-job
          (ediff-with-current-buffer ediff-buffer-C
            (setcar mode-line-format
                    `(:propertize ,(replace-regexp-in-string
                                    "^ " "" (car mode-line-format))
                                  face mode-line-separator))
            (setcar (cdr mode-line-format)
                    `(:propertize ,(cadr mode-line-format)
                                  face mode-line-separator))
            (setq mode-line-format-raw
                  `("" mode-line-front-space
                    ediff-mode-line-marker ,mode-line-format))
            (setq mode-line-format
                  (mode-line-format-auto-truncate mode-line-format-raw))))
      (if (ediff-buffer-live-p ediff-ancestor-buffer)
          (ediff-with-current-buffer ediff-ancestor-buffer
            (setcar mode-line-format
                    `(:propertize ,(replace-regexp-in-string
                                    "^ " "" (car mode-line-format))
                                  face mode-line-separator))
            (setcar (cdr mode-line-format)
                    `(:propertize ,(cadr mode-line-format)
                                  face mode-line-separator))
            (setq mode-line-format-raw
                  `("" mode-line-front-space
                    ediff-mode-line-marker ,mode-line-format))
            (setq mode-line-format
                  (mode-line-format-auto-truncate mode-line-format-raw))))

      (setq ediff-modifying-mode-line-format nil)))

  (mapc (lambda (symbol)
          (advice-add symbol :around #'ediff-modify-mode-line-format))
        '(ediff-strip-mode-line-format ediff-refresh-mode-lines)))

(advice-add 'balance-mode-on :around (lambda (function &rest args)
                                       (unless (eq major-mode #'ediff-mode)
                                         (apply function args))))

(overriding-set-key (kbd "C-c d f") #'ediff-files)
(overriding-set-key (kbd "C-c d b") #'ediff-buffers)
(overriding-set-key (kbd "C-c d r") #'ediff-regions-linewise)


(resolve init-ediff)
