
;;;; init-server.el


(when (daemonp)
  (defun server-init-daemon-at-first-make-frame (frame)
    (select-frame frame)
    (keyboard-translate ?\C-h ?\C-?)
    (remove-hook 'after-make-frame-functions
                 'server-init-daemon-at-first-make-frame))

  (add-hook 'after-make-frame-functions
            'server-init-daemon-at-first-make-frame))
