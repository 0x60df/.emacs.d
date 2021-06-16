
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

  (modify-minor-mode-lighter 'isearch-mode '((-3 "" isearch-mode)
                                             (demi-view-isearch-mode
                                              demi-view-isearch-mode-lighter))))


(resolve init-isearch)
