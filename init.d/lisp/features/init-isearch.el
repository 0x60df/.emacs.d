;;; -*- lexical-binding: t -*-
;;;; init-isearch.el


(premise init)
(premise mode-line)

(defvar demi-view-isearch-mode)
(declare-function demi-view-isearch-mode load-file-name t t)

(push '(isearch-mode . 20) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'isearch
  (defconst demi-view-isearch-mode-bindings
    (mapcar (lambda (key-command)
              (list (car key-command) (cdr key-command)
                    (lookup-key isearch-mode-map (car key-command))))
            `((,(kbd "s") . isearch-repeat-forward)
              (,(kbd "r") . isearch-repeat-backward)))
    "List of key, binding and its original for demi-`view-isearch-mode'.")

  (defvar demi-view-isearch-mode-lighter ":V"
    "Lighter string for demi-`view-isearch-mode'.")

  (define-minor-mode demi-view-isearch-mode
    "Minor mode to view hits but not printing char."
    :global t
    (if demi-view-isearch-mode
        (dolist (key-command-original demi-view-isearch-mode-bindings)
          (define-key isearch-mode-map
            (car key-command-original)
            (cadr key-command-original)))
      (dolist (key-command-original demi-view-isearch-mode-bindings)
          (define-key isearch-mode-map
            (car key-command-original)
            (caddr key-command-original)))))

  (define-key isearch-mode-map (kbd "C-]") #'demi-view-isearch-mode)

  (add-hook 'isearch-mode-end-hook (lambda () (demi-view-isearch-mode 0)))

  (let* ((buffer nil)
         (minor-mode-width nil)
         (column-number-mode-flag nil)
         (demi-view-isearch-mode-format
          '(demi-view-isearch-mode demi-view-isearch-mode-lighter))
         (hook
          (lambda ()
            (setq buffer nil)
            (setq minor-mode-width nil)
            (setq column-number-mode-flag nil)

            (mode-line-buffer-identification-shrink-mode)
            (mode-line-mode-name-shrink-mode)
            (mode-line-minor-mode-shrink-mode)
            (mode-line-vc-mode-shrink-mode)

            (setq column-number-mode-flag column-number-mode)
            (column-number-mode 0)

            (setq buffer (current-buffer))
            (if (local-variable-p 'mode-line-minor-mode-shrink-width)
                (setq minor-mode-width mode-line-minor-mode-shrink-width))
            (setq mode-line-minor-mode-shrink-width 0)

            (let ((cell (member "%]" mode-line-modes)))
              (when cell
                (setcdr cell (cons (car cell) (cdr cell)))
                (setcar cell demi-view-isearch-mode-format)))))
         (end-hook
          (lambda ()
            (setq mode-line-modes
                  (delq demi-view-isearch-mode-format mode-line-modes))

            (unwind-protect
                (mode-line-minor-mode-shrink-mode 0)
              (if (buffer-live-p buffer)
                  (if minor-mode-width
                      (setq mode-line-minor-mode-shrink-width minor-mode-width)
                    (with-current-buffer buffer
                      (kill-local-variable
                       'mode-line-minor-mode-shrink-width))))
              (setq buffer nil)
              (setq minor-mode-width nil))

            (if column-number-mode-flag
                (unwind-protect
                    (column-number-mode)
                  (setq column-number-mode-flag nil)))

            (mode-line-buffer-identification-shrink-mode 0)
            (mode-line-mode-name-shrink-mode 0)
            (mode-line-vc-mode-shrink-mode 0))))
    (add-hook 'isearch-mode-hook hook)
    (add-hook 'isearch-mode-end-hook end-hook))

  (modify-minor-mode-lighter 'isearch-mode '((-3 "" isearch-mode)
                                             (demi-view-isearch-mode
                                              demi-view-isearch-mode-lighter))))


(resolve init-isearch)
