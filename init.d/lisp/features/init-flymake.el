
;;;; init-flymake.el


(premise init)
(premise bindings)
(premise mode-line)

(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(declare-function flymake--overlays "flymake")

(declare-function flymake-show-help load-file-name t t)
(declare-function flymake--modify-mode-line-format load-file-name t t)

(push '(flymake -mode . 44) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-c #") #'flymake-mode)
(overriding-set-key (kbd "C-l 3") #'flymake-mode)

(with-eval-after-load 'flymake
  (defun flymake-show-help ()
    "Show help stirng in echo area."
    (interactive)
    (let* ((ovs (flymake--overlays :beg (point)
                                   :compare #'>
                                   :key #'overlay-start))
           (target (car ovs)))
      (if target
          (message
           "%s"
           (funcall (overlay-get target 'help-echo)
                    (selected-window) target (overlay-start target))))))

  (defun flymake--modify-mode-line-format (return)
    "Advising `flymake--mode-line-format' to modify return."
    (if (listp return)
        (mapcar (lambda (element)
                  (if (not (eq (car element) :propertize))
                      element
                    (if (equal " Flymake" (cadr element))
                        (setcar (cdr element) " FlyM"))
                    (mapc (lambda (prop)
                            (if (plist-get element prop)
                                (plist-put element prop nil)))
                          '(mouse-face keymap help-echo))
                    element))
                return)
      return))

  (advice-add 'flymake--mode-line-format
              :filter-return #'flymake--modify-mode-line-format)

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
        overriding-reserved-key-map-alist))


(resolve init-flymake)
