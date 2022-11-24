
;;;; init-wdired.el


(premise init)
(premise bindings)

(eval-when-compile (require 'dired))

(with-eval-after-load 'dired
  (define-key dired-mode-map "r" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'wdired
  (add-hook 'wdired-mode-hook (lambda ()
                                (if wdired-mode-hook
                                    (progn
                                      (balance-weight-mode 0)
                                      (balance-mode 0)
                                      (balance-mode-implement-keys
                                       (list (kbd "C-c C-c"))
                                       overriding-balance-weight-mode-map)
                                      (balance-mode-implement-keys
                                       (list (kbd "C-c C-c"))
                                       overriding-balance-mode-map))
                                  (define-key
                                    overriding-balance-mode-map
                                    (kbd "c c")
                                    nil)
                                  (define-key
                                    overriding-balance-weight-mode-map
                                    (kbd "c c")
                                    nil)
                                  (balance-mode 0)
                                  (balance-weight-mode 1)))))


(resolve init-wdired)
