
;;;; init-cc-mode.el


(premise init)

(eval-when-compile (require 'cc-mode))

(with-eval-after-load 'cc-vars
  (add-hook 'c-mode-common-hook (lambda ()
                                  (c-set-style "gnu")
	                          (setq c-hungry-delete-key t))))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "C-c c c") #'compile)
  (define-key c-mode-map (kbd "C-c c b") #'gdb)

  (define-key c++-mode-map (kbd "C-c c c") #'compile)
  (define-key c++-mode-map (kbd "C-c c b") #'gdb))


(resolve init-cc-mode)
