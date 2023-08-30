
;;;; init-vterm.el


(premise init)
(premise inst-vterm)
(premise bindings)

(defvar vterm-mode-map)

(declare-function vterm-send "vterm")

(define-minor-mode overriding-vterm-ctl-o-mode
  "Minor mode to enable overriding keymap for C-o."
  :keymap `((,(kbd "C-o") . ,(lambda () (interactive) (vterm-send "C-o")))))

(defun vterm-emulate-control-key-event ()
  "Emulate a event generated from last event and control modifier."
  (interactive)
  (let* ((last-event last-command-event)
         (modifiers (event-modifiers last-event))
         (basic-type (event-basic-type last-event)))
    (unless (memq 'control modifiers)
      (setq modifiers (cons 'control modifiers)))
    (setq unread-command-events
          (cons (event-convert-list (append modifiers (list basic-type)))
                unread-command-events))))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (if (init-unit-p bindings)
      (add-hook 'vterm-mode-hook (lambda () (balance-mode 0))))

  (push `(overriding-vterm-ctl-o-mode . ,overriding-vterm-ctl-o-mode-map)
        overriding-reserved-key-map-alist)
  (add-hook 'vterm-mode-hook #'overriding-vterm-ctl-o-mode)

  (define-key vterm-mode-map (kbd "C-g")
              (lambda () (interactive) (vterm-send "C-g")))
  (add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (if (eq major-mode #'vterm-mode)
                  (mapc (lambda (c)
                          (define-key overriding-balance-mode-map
                                      (kbd (char-to-string c))
                                      #'vterm-emulate-control-key-event))
                        "abcdefghjklmnptuwyz"))))
  (advice-add 'balance-mode-update-keys
              :before (lambda (&rest _)
                        (if (and (eq major-mode #'vterm-mode)
                                 balance-mode
                                 overriding-vterm-ctl-o-mode)
                            (overriding-vterm-ctl-o-mode -1))))
  (add-hook 'balance-mode-hook
            (lambda ()
              (if (and (eq major-mode #'vterm-mode)
                       (not balance-mode)
                       (not overriding-vterm-ctl-o-mode))
                  (overriding-vterm-ctl-o-mode)))))


(resolve init-vterm)
