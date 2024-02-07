
;;;; init-wdired.el


(premise init)
(premise bindings)

(eval-when-compile (require 'dired))

(with-eval-after-load 'dired
  (define-key dired-mode-map "w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'wdired
  (advice-add 'balance-mode-context :filter-return
              (lambda (ret)
                (if (eq major-mode 'wdired-mode)
                    #'balance-mode
                  ret)))
  (add-hook 'wdired-mode-hook (lambda ()
                                (balance-weight-mode 0)
                                (balance-mode 1)
                                (balance-mode-implement-keys
                                 (list (kbd "C-c C-c")
                                       (kbd "C-c C-k"))
                                 overriding-balance-mode-map)))
  (advice-add 'wdired-finish-edit :after (lambda (&rest _)
                                           (balance-mode-update-keys)
                                           (balance-mode 0)
                                           (balance-weight-mode 1)))
  (advice-add 'wdired-abort-changes :after (lambda (&rest _)
                                             (balance-mode-update-keys)
                                             (balance-mode 0)
                                             (balance-weight-mode 1))))


(resolve init-wdired)
