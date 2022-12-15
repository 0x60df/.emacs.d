
;;;; init-wdired.el


(premise init)
(premise bindings)

(eval-when-compile (require 'dired))

(with-eval-after-load 'dired
  (define-key dired-mode-map "w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'wdired
  (add-hook 'wdired-mode-hook (lambda ()
                                (if wdired-mode-hook
                                    (progn
                                      (balance-weight-mode 0)
                                      (balance-mode 0)
                                      (balance-mode-implement-keys
                                       (list (kbd "C-c C-c")
                                             (kbd "C-c C-k")
                                             (kbd "C-f")
                                             (kbd "C-b")
                                             (kbd "C-n")
                                             (kbd "C-p")
                                             (kbd "C-e"))
                                       overriding-balance-weight-mode-map)
                                      (balance-mode-implement-keys
                                       (list (kbd "C-c C-c")
                                             (kbd "C-c C-k"))
                                       overriding-balance-mode-map)))))
  (advice-add 'wdired-finish-edit :after (lambda (&rest _)
                                           (balance-mode-update-keys)
                                           (balance-mode 0)
                                            (balance-weight-mode 1)))
  (advice-add 'wdired-abort-changes :after (lambda (&rest _)
                                             (balance-mode-update-keys)
                                             (balance-mode 0)
                                             (balance-weight-mode 1))))


(resolve init-wdired)
