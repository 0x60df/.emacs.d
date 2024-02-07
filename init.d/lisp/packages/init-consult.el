;;; -*- lexical-binding: t -*-
;;;; init-consult.el


(premise init)
(premise bindings)
(premise subr)
(premise inst-consult)

(eval-when-compile (require 'consult))

(declare-function consult--customize-put "consult")
(declare-function consult--multi-narrow "consult")
(declare-function consult--multi-enabled-sources "consult")

(overriding-set-key (kbd "C-o C-o") #'open-line)
(overriding-set-key (kbd "C-o C-k") #'consult-yank-from-kill-ring)
(overriding-set-key (kbd "C-o C-s") #'consult-line)
(overriding-set-key (kbd "C-o C-j") #'consult-buffer)
(overriding-set-key (kbd "C-o r") #'consult-register)
(overriding-set-key (kbd "C-o h") #'consult-outline)
(overriding-set-key (kbd "C-o i") #'consult-imenu)
(overriding-set-key (kbd "C-o g") #'consult-goto-line)
(overriding-set-key (kbd "C-o m") #'consult-mark)
(overriding-set-key (kbd "M-y") #'consult-yank-pop)

(dolist (key (list (kbd "C-o C-o")
                   (kbd "C-o C-k")
                   (kbd "C-o C-s")
                   (kbd "C-o C-j")
                   (kbd "C-o r")
                   (kbd "C-o h")
                   (kbd "C-o i")
                   (kbd "C-o g")
                   (kbd "C-o m")))
  (add-to-list 'balance-mode-key-list key))

(dolist (key-alias (list `(,(kbd "o SPC r") . ,(kbd "o r"))
                         `(,(kbd "o SPC h") . ,(kbd "o h"))
                         `(,(kbd "o SPC i") . ,(kbd "o i"))
                         `(,(kbd "o SPC g") . ,(kbd "o g"))
                         `(,(kbd "o SPC m") . ,(kbd "o m"))))
  (add-to-list 'balance-mode-key-alias-alist key-alias))

(add-hook 'balance-mode-update-keys-hook
            (lambda ()
              (when (or (string-equal (buffer-name) "*Messages*")
                        (eq major-mode #'help-mode)
                        (eq major-mode 'emacs-lisp-compilation-mode)
                        (eq major-mode #'dired-mode)
                        (eq major-mode 'org-agenda-mode)
                        (eq major-mode 'magit-status-mode))
                (let ((entry (lookup-key (current-local-map) (kbd "o"))))
                  (if (and entry (not (numberp entry)))
                      (define-key overriding-balance-weight-mode-map
                        (kbd "oo") entry)))

                (balance-mode-implement-keys
                 (list (kbd "C-o C-j"))
                 overriding-balance-weight-mode-map))))

(custom-set-variables
 '(register-preview-delay 0.5)
 '(consult-narrow-key "<"))

(setq register-preview-function #'consult-register-format)

(custom-set-variables
 '(xref-show-xrefs-function #'consult-xref)
 '(xref-show-definitions-function #'consult-xref))

(with-eval-after-load 'consult
  (consult-customize consult-buffer :preview-key "M-."))


(define-minor-mode inferior-org-mode
    "Enabled when major-mode is `org-mode'."
    :group 'user)

(defvar-local balance-mode-inferior-org nil
    "`inferior-org-mode' but t if `balance-mode' is active.")

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'inferior-org-mode)

  (defvar overriding-org-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-o h") #'consult-org-heading)
      map)
    "Keymap for org-mode which overrides overriding bindings.")

  (push `(inferior-org-mode . ,overriding-org-mode-map)
        overriding-reserved-key-map-alist)

  (defvar balance-mode-overriding-inferior-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "o h") #'consult-org-heading)
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
                           (when (seq-find (lambda (imenu-entry)
                                           (equal
                                            (replace-regexp-in-string
                                             "^.* " "" (car imenu-entry))
                                            token))
                                           (car args))
                             (add-hook-for-once
                              'post-command-hook
                              (lambda ()
                                (if (minibufferp)
                                    (let ((exit (key-binding (kbd "RET"))))
                                      (if (functionp exit)
                                          (funcall exit))))))
                             token))))))))
       (unwind-protect
           (progn
             (advice-add 'consult--read :filter-args advice)
             (apply old-function args))
         (advice-remove 'consult--read advice))))))


(resolve init-consult)
