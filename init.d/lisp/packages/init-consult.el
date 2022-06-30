
;;;; init-consult.el


(premise init)
(premise bindings)
(premise inst-consult)

(eval-when-compile (require 'consult))

(declare-function consult--customize-put "consult")

(overriding-set-key (kbd "C-q C-q") #'quoted-insert)
(overriding-set-key (kbd "C-q C-j") #'consult-buffer)
(overriding-set-key (kbd "C-q C-y") #'consult-yank-from-kill-ring)
(overriding-set-key (kbd "C-q C-r") #'consult-register)
(overriding-set-key (kbd "C-q C-o") #'consult-outline)
(overriding-set-key (kbd "C-q C-i") #'consult-imenu)
(overriding-set-key (kbd "C-q C-g") #'consult-goto-line)
(overriding-set-key (kbd "C-q C-m") #'consult-mark)
(overriding-set-key (kbd "C-q C-s") #'consult-line)
(overriding-set-key (kbd "M-y") #'consult-yank-pop)

(dolist (key (list (kbd "C-q C-q")
                   (kbd "C-q C-j")
                   (kbd "C-q C-y")
                   (kbd "C-q C-r")
                   (kbd "C-q C-o")
                   (kbd "C-q C-i")
                   (kbd "C-q C-g")
                   (kbd "C-q C-m")
                   (kbd "C-q C-s")))
  (add-to-list 'balance-mode-key-list key))

(add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (when (or (string-equal (buffer-name) "*Messages*")
                        (eq major-mode #'help-mode)
                        (eq major-mode 'emacs-lisp-compilation-mode)
                        (eq major-mode #'dired-mode)
                        (eq major-mode 'org-agenda-mode)
                        (eq major-mode 'magit-status-mode))
                (let ((entry (lookup-key (current-local-map) (kbd "q"))))
                  (if (and entry (not (numberp entry)))
                      (define-key overriding-balance-weight-mode-map
                        (kbd "qq") entry)))

                (balance-mode-implement-keys
                 (list (kbd "C-q C-j"))
                 overriding-balance-weight-mode-map))))

(custom-set-variables
 '(register-preview-delay 0.5)
 '(consult-narrow-key "<"))

(setq register-preview-function #'consult-register-format)

(custom-set-variables
 '(xref-show-xrefs-function #'consult-xref)
 '(xref-show-definitions-function #'consult-xref))

(with-eval-after-load 'consult
  (consult-customize consult-buffer :preview-key (kbd "M-.")))


(define-minor-mode inferior-org-mode
    "Enabled when major-mode is `org-mode'."
    :group 'user)

(defvar-local balance-mode-inferior-org nil
    "`inferior-org-mode' but t if `balance-mode' is active.")

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'inferior-org-mode)

  (defvar overriding-org-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-q C-o") #'consult-org-heading)
      map)
    "Keymap for org-mode which overrides overriding bindings.")

  (push `(inferior-org-mode . ,overriding-org-mode-map)
        overriding-reserved-key-map-alist)

  (defvar balance-mode-overriding-inferior-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q o") #'consult-org-heading)
      map)
    "Overriding keymap for org-mode with balance mode.")

  (add-hook 'inferior-org-mode-hook
            (lambda ()
              (if (and inferior-org-mode balance-mode)
                  (setq balance-mode-inferior-org t)
                (setq balance-mode-inferior-org nil))))
  (add-hook 'balance-mode-hook (lambda ()
                                 (if balance-mode
                                     (if inferior-org-mode
                                         (setq balance-mode-inferior-org t))
                                   (setq balance-mode-inferior-org nil))))

  (balance-mode-add-to-map-alist
   `(balance-mode-inferior-org . ,balance-mode-overriding-inferior-mode-map)))


(resolve init-consult)
