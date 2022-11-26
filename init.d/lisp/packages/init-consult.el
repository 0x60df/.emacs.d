;;; -*- lexical-binding: t -*-
;;;; init-consult.el


(premise init)
(premise bindings)
(premise inst-consult)

(eval-when-compile (require 'consult))

(declare-function consult--customize-put "consult")

(overriding-set-key (kbd "C-q C-q") #'quoted-insert)
(overriding-set-key (kbd "C-q C-y") #'consult-yank-from-kill-ring)
(overriding-set-key (kbd "C-q C-s") #'consult-line)
(overriding-set-key (kbd "C-q C-j") #'consult-buffer)
(overriding-set-key (kbd "C-q r") #'consult-register)
(overriding-set-key (kbd "C-q o") #'consult-outline)
(overriding-set-key (kbd "C-q i") #'consult-imenu)
(overriding-set-key (kbd "C-q g") #'consult-goto-line)
(overriding-set-key (kbd "C-q m") #'consult-mark)
(overriding-set-key (kbd "M-y") #'consult-yank-pop)

(dolist (key (list (kbd "C-q C-q")
                   (kbd "C-q C-y")
                   (kbd "C-q C-s")
                   (kbd "C-q C-j")
                   (kbd "C-q r")
                   (kbd "C-q o")
                   (kbd "C-q i")
                   (kbd "C-q g")
                   (kbd "C-q m")))
  (add-to-list 'balance-mode-key-list key))

(dolist (key-alias (list `(,(kbd "q SPC r") . ,(kbd "q r"))
                         `(,(kbd "q SPC o") . ,(kbd "q o"))
                         `(,(kbd "q SPC i") . ,(kbd "q i"))
                         `(,(kbd "q SPC g") . ,(kbd "q g"))
                         `(,(kbd "q SPC m") . ,(kbd "q m"))))
  (add-to-list 'balance-mode-key-alias-alist key-alias))

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
      (define-key map (kbd "C-q o") #'consult-org-heading)
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

(defvar consult-pick-narrow-sources nil
  "Temporary sources holder for `consult-pick-narrow'.")

(defun consult-pick-narrow ()
  "Prompt user narrow keys and apply it."
  (interactive)
  (if (and (minibufferp)
           consult-pick-narrow-sources)
      (let* ((sources (consult--multi-enabled-sources
                       consult-pick-narrow-sources))
             (narrow (consult--multi-narrow sources))
             (key (read-key
                   (format " %s "
                           (mapconcat (lambda (a)
                                        (key-description (vector (car a))))
                                      narrow " "))))
             (empty (= 0 (length (minibuffer-contents)))))
        (setq unread-command-events
              (append (unless empty
                        (listify-key-sequence (kbd "C-a C-k")))
                      (listify-key-sequence (kbd "DEL"))
                      (if (assoc key narrow)
                          (listify-key-sequence (vector key ?\s)))
                      (unless empty
                        (listify-key-sequence (kbd "C-y"))))))))

(with-eval-after-load 'consult
  (advice-add 'consult--multi :around
              (lambda (fun sources &rest args)
                (setq consult-pick-narrow-sources sources)
                (unwind-protect
                    (apply fun sources args)
                  (setq consult-pick-narrow-sources nil))))
  (define-key consult-narrow-map (kbd "C-o") #'consult-pick-narrow)

  (advice-add
   'consult-imenu
   :around
   (lambda (old-function &rest args)
     (let ((advice
            (lambda (args)
              (append args
                      (list
                       :initial
                       (let (begin end)
                         (save-excursion
                           (skip-syntax-backward "w_")
                           (setq begin (point))
                           (skip-syntax-forward "w_")
                           (setq end (point)))
                         (let ((token
                                (buffer-substring-no-properties begin end)))
                           (if (seq-find (lambda (imenu-entry)
                                           (equal
                                            (replace-regexp-in-string
                                             "^.* " "" (car imenu-entry))
                                            token))
                                         (car args))
                               token))))))))
       (unwind-protect
           (progn
             (advice-add 'consult--read :filter-args advice)
             (apply old-function args))
         (advice-remove 'consult--read advice))))))


(resolve init-consult)
