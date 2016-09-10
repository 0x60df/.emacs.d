
;;;; init-server.el


(when (daemonp)
  (add-hook 'server-visit-hook
            (lambda () (keyboard-translate ?\C-h ?\C-?)))

  (defun server-init-daemon-at-first-make-frame (frame)
    (select-frame frame)
    (remove-hook 'after-make-frame-functions
                 'server-init-daemon-at-first-make-frame))

  (add-hook 'after-make-frame-functions
            'server-init-daemon-at-first-make-frame))
