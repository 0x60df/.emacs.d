
;;;; init-fmmm.el


(premise init)
(premise user-feature)

(declare-function fmmm-update-major-mode-on-autoload-list "fmmm")
(declare-function fmmm-update-minor-mode-on-autoload-list "fmmm")
(declare-function fmmm-save-cache "fmmm")

(with-eval-after-load 'fmmm
  (defvar fmmm-cache-auto-save-timer nil
    "Timer used to automatically save the fmmm cache.")
  (setq fmmm-cache-auto-save-timer
        (run-with-idle-timer
         (* 60 62) t (lambda ()
                       (fmmm-update-major-mode-on-autoload-list)
                       (fmmm-update-minor-mode-on-autoload-list)
                       (fmmm-save-cache))))
  (fmmm-autoload-collector-mode))


(resolve init-fmmm)
