
;;;; scratchb.el

(premise init)

(require 'scratchb)
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (use-local-map (copy-keymap lisp-interaction-mode-map))
              (local-set-key (kbd "ESC ESC DEL") #'scratchb-flush))))

(add-hook 'scratchb-after-revert-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (use-local-map (copy-keymap lisp-interaction-mode-map))
              (local-set-key (kbd "ESC ESC DEL") #'scratchb-flush))))

(add-hook 'scratchb-before-flush-hook #'scratchb-snapshot)
(add-hook 'kill-emacs-hook #'scratchb-snapshot)
(add-hook 'kill-buffer-hook
          (lambda ()
            (if (equal "*scratch*" (buffer-name (current-buffer)))
                (scratchb-snapshot))))

(add-hook 'buffer-list-update-hook #'scratchb-revert)


(resolve scratchb)
