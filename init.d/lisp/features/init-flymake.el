
;;;; init-flymake.el


(premise init)
(premise bindings)
(premise mode-line)

(eval-when-compile (require 'flymake))

(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(if (version< emacs-version "30")
    (declare-function flymake--overlays "flymake"))

(declare-function flymake-show-help load-file-name t t)
(declare-function flymake--modify-mode-line-format load-file-name t t)

(push '(flymake-mode . 43) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-c #") #'flymake-mode)
(overriding-set-key (kbd "C-l 3") #'flymake-mode)

(add-to-list 'balance-mode-key-list (kbd "C-l 3"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC 3") . ,(kbd "l 3")))

(defvar-local balance-mode-flymake nil
  "`flymake-mode' but t if `balance-mode' is active.")

(if (daemonp)
    (custom-set-variables
     '(flymake-indicator-type 'fringes)))

(with-eval-after-load 'flymake
  (defun flymake--overlays-at (pos)
    "Get flymake-related overlays at POS."
    (cl-loop
     for o in (overlays-at pos)
     when (overlay-get o 'flymake-diagnostic)
     collect o))

  (defun flymake-show-help ()
    "Show help stirng in echo area."
    (interactive)
    (let* ((ovs (if (version< emacs-version "30")
                    (flymake--overlays :beg (point)
                                       :compare #'>
                                       :key #'overlay-start)
                  (cl-sort (flymake--overlays-at (point)) #'>
                           :key #'overlay-start)))
           (target (car ovs)))
      (if target
          (message
           "%s"
           (funcall (overlay-get target 'help-echo)
                    (selected-window) target (overlay-start target))))))

  (defvar flymake-last-help nil "Last help message for flymake auto show help.")
  (add-hook 'pre-command-hook
            (lambda ()
              (when (and (eldoc--message-command-p this-command)
                         (null eldoc-last-message)
                         flymake-last-help)
                (message flymake-last-help))))
  (add-hook 'post-command-hook
            (lambda ()
              (run-with-idle-timer
               0.5 nil (lambda ()
                         (if (null eldoc-last-message)
                             (if (if (version< emacs-version "30")
                                     (flymake--overlays :beg (point))
                                   (flymake--overlays-at (point)))
                                 (setq flymake-last-help (flymake-show-help))
                               (if (eldoc--message-command-p last-command)
                                   (message nil))
                               (setq flymake-last-help nil))
                           (setq flymake-last-help nil))))))

  (advice-add 'flymake--mode-line-title :filter-return (lambda (return) "FlyM"))

  (defvar overriding-flymake-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c #") (make-sparse-keymap))
      (define-key map (kbd "C-c # #") #'flymake-mode)
      (define-key map (kbd "C-l # n") #'flymake-goto-next-error)
      (define-key map (kbd "C-l # p") #'flymake-goto-prev-error)

      (define-key map (kbd "C-l 3") (make-sparse-keymap))
      (define-key map (kbd "C-l 3 3") #'flymake-mode)
      (define-key map (kbd "C-l 3 n") #'flymake-goto-next-error)
      (define-key map (kbd "C-l 3 p") #'flymake-goto-prev-error)

      (define-key map (kbd "H-3") #'flymake-show-help)
      (define-key map (kbd "s->") #'flymake-goto-next-error)
      (define-key map (kbd "s-<") #'flymake-goto-prev-error)
      map)
    "Keymap for `flymake-mode' which overrides global overriding maps.")

  (push `(flymake-mode . ,overriding-flymake-mode-map)
        overriding-reserved-key-map-alist)

  (defvar balance-mode-overriding-flymake-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c #") (make-sparse-keymap))
      (define-key map (kbd "c # #") #'flymake-mode)
      (define-key map (kbd "l # n") #'flymake-goto-next-error)
      (define-key map (kbd "l # p") #'flymake-goto-prev-error)

      (define-key map (kbd "l 3") (make-sparse-keymap))
      (define-key map (kbd "l 3 3") #'flymake-mode)
      (define-key map (kbd "l 3 n") #'flymake-goto-next-error)
      (define-key map (kbd "l 3 p") #'flymake-goto-prev-error)
      map)
    "Keymap for `flymake-mode' which overrides global overriding maps.")

  (add-hook 'flymake-mode-hook (lambda ()
                                  (if (and flymake-mode balance-mode)
                                      (setq balance-mode-flymake t)
                                    (setq balance-mode-flymake nil))))
  (add-hook 'balance-mode-hook (lambda ()
                                 (if balance-mode
                                     (if flymake-mode
                                         (setq balance-mode-flymake t))
                                   (setq balance-mode-flymake nil))))

  (balance-mode-add-to-map-alist
   `(balance-mode-flymake . ,balance-mode-overriding-flymake-mode-map)))


(resolve init-flymake)
