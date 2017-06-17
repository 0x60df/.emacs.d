
;;;; init-scratchb.el

(premise init)

(scratchb-auto-revert-mode 1)
(scratchb-auto-snapshot-mode 1)

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


(resolve scratchb)
