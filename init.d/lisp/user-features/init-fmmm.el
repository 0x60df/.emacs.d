
;;;; init-fmmm.el


(premise init)

(require 'fmmm)

(custom-set-variables '(fmmm-complementary-major-mode
                        '(shell-script-mode ruby-mode)))

(defvar fmmm-cache-auto-save-timer)
(setq fmmm-cache-auto-save-timer
      (run-with-idle-timer
       80 t (lambda ()
              (fmmm-update-major-mode-on-autoload-list)
              (fmmm-update-minor-mode-on-autoload-list)
              (fmmm-save-cache))))
(fmmm-autoload-collector-mode 1)

(resolve fmmm)
