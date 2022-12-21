;;; -*- lexical-binding: t -*-
;;;; init-multi-vterm.el


(premise init)
(premise inst-multi-vterm)
(premise bindings)
(premise keyboard)

(defvar vterm-mode-map)

(defvar-local multi-vterm-dedicated-frame nil
  "Dedicated frame for `vterm' buffer.")

(defcustom vterm-window-fringe-width 0
  "Width of friges when frame configuration is toggled."
  :group 'user
  :type 'number)

(defcustom vterm-frame-height 24
  "Height of frame when frame configuration is toggled."
  :group 'user
  :type 'number)

(defun multi-vterm-with-dedicated-frame-configuration ()
  "`multi-vterm' and set up frame to dedicated configuration."
  (interactive)
  (call-interactively #'multi-vterm)
  (set-window-fringes nil
                      vterm-window-fringe-width vterm-window-fringe-width
                      nil nil)
  (set-frame-height nil vterm-frame-height)
  (setq multi-vterm-dedicated-frame (selected-frame))
  (setq mode-line-format nil))

(defun multi-vterm-toggle-mode-line ()
  "Toggle mode-line-format of vterm buffer."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (if mode-line-format
        (setq mode-line-format nil)
      (setq mode-line-format
            (mode-line-format-auto-truncate mode-line-format-raw))
      (force-mode-line-update))))

(with-eval-after-load 'multi-vterm
  (advice-add 'multi-vterm-handle-close
              :after
              (lambda (&rest _)
                (let* ((process (get-buffer-process (current-buffer)))
                       (sentinel (process-sentinel process)))
                  (set-process-sentinel
                   process
                   (lambda (proc change)
                     (let ((dedicated-frame
                            (buffer-local-value 'multi-vterm-dedicated-frame
                                                (process-buffer proc))))
                       (funcall sentinel proc change)
                       (if (and dedicated-frame
                                (frame-live-p dedicated-frame)
                                (= 1 (length (window-list dedicated-frame))))
                           (delete-frame dedicated-frame)))))))))


(overriding-set-key (kbd "C-l t") #'multi-vterm)
(overriding-set-key (kbd "H-t") #'multi-vterm)
(overriding-set-key (kbd "C-H-t")
                    #'multi-vterm-with-dedicated-frame-configuration)

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-@") #'multi-vterm-toggle-mode-line))

(add-to-list 'balance-mode-key-list (kbd "C-l t"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC t") . ,(kbd "l t")))
(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (if (or (memq major-mode '(help-mode
                                       dired-mode
                                       emacs-lisp-compilation-mode
                                       org-agenda-mode))
                    (string-equal (buffer-name) "*Messages*"))
                (let* ((key (kbd "t"))
                       (binding (lookup-key overriding-balance-mode-map key)))
                  (unless (numberp binding)
                    (define-key overriding-balance-mode-map key
                      (lambda ()
                        (interactive)
                        (if balance-mode-transient-hyper
                            (progn
                              (balance-mode 0)
                              (balance-weight-mode 1)
                              (setq unread-command-events
                                    (append (kbd (concat "H-" key)) nil)))
                          (if (commandp binding)
                              (call-interactively binding)))))))))
          100)


(resolve init-multi-vterm)
