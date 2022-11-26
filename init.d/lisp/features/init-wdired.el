
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
                                      (echo (key-binding (kbd "C-f")))
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
                                       overriding-balance-mode-map))
                                  (dolist (key (list (kbd "c c")
                                                     (kbd "c k")))
                                    (define-key
                                      overriding-balance-mode-map
                                      key
                                      nil))
                                  (dolist (key (list (kbd "c c")
                                                     (kbd "c k")
                                                     (kbd "f")
                                                     (kbd "b")
                                                     (kbd "n")
                                                     (kbd "p")
                                                     (kbd "e")))
                                    (define-key
                                      overriding-balance-weight-mode-map
                                      key
                                      nil))
                                  (balance-mode 0)
                                  (balance-weight-mode 1)))))


(resolve init-wdired)
