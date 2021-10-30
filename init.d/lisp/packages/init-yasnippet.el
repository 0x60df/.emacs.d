
;;;; init-yasnippet.el


(premise init)
(premise custom)
(premise subr)
(premise mode-line)
(premise bindings)
(premise inst-yasnippet)

(eval-when-compile (require 'yasnippet))

(declare-function yas-reload-all "yasnippet")
(declare-function yas-expand "yasnippet")
(declare-function yas-insert-snippet "yasnippet")
(declare-function yas-describe-tables "yasnippet")
(declare-function yas--field-probably-deleted-p "yasnippet")
(declare-function yas-exit-snippet "yasnippet")

(declare-function yas--find-next-field-boundary-guard load-file-name t t)

(custom-set-variables
 '(yas-prompt-functions nil)
 '(yas-use-menu nil)
 '(yas-prompt-functions '(yas-completing-prompt
                          yas-maybe-ido-prompt
                          yas-no-prompt)))

(push '(yas-minor-mode . 31) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'yasnippet
  (yas-reload-all)

  (modify-minor-mode-lighter 'yas-minor-mode " YaS")

  (defun yas--find-next-field-boundary-guard
      (yas--find-next-field n snippet active &rest args)
    "Advising `yas--find-next-field' to guard boundary overflow.
If return value of original function is nil, return the
head or tail of the live fields according to sign of N.
Otherwise, return original one."
    (let ((next-field (apply yas--find-next-field n snippet active args)))
      (if next-field
          next-field
        (let ((live-fields
               (seq-filter
                (lambda (field)
                  (or (eq field active)
                      (not (yas--field-probably-deleted-p snippet field))))
                (yas--snippet-fields snippet))))
          (car (if (< n 0)
                   live-fields
                 (reverse live-fields)))))))

  (advice-add 'yas--find-next-field
              :around #'yas--find-next-field-boundary-guard)

  (define-key yas-keymap (kbd "RET") #'yas-exit-snippet)

  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (defvar overriding-yas-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-'") #'yas-expand)
      (define-key map (kbd "C-c y") #'yas-insert-snippet)
      (define-key map (kbd "C-c Y") #'yas-describe-tables)
      map)
    "Keymap for `yas-minor-mode' which overrides global overriding maps.")

  (push `(yas-minor-mode . ,overriding-yas-minor-mode-map)
        overriding-reserved-key-map-alist))

(add-hook 'emacs-startup-hook
          (lambda ()
            (mapc (lambda (hook)
                    (add-hook hook #'yas-minor-mode))
                  '(prog-mode-hook
                    html-mode-hook
                    org-mode-hook))

            (with-current-buffer "*scratch*"
              (let ((hook (make-local-variable 'first-change-hook)))
                (add-hook-for-once
                 hook
                 (lambda ()
                   (when (seq-some #'derived-mode-p
                                   '(prog-mode
                                     org-mode
                                     html-mode))
                     (unless (featurep 'yasnippet)
                       (message "[yas] Loading the snippet tables."))
                     (yas-minor-mode))))))))


(resolve init-yasnippet)
