
;;;; init-yasnippet.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise feature)
(premise inst-yasnippet)

(eval-when-compile (require 'yasnippet))

(lazy-autoload 'yas-reload-all "yasnippet")
(declare-function yas-expand "yasnippet")

(declare-function yas--field-probably-deleted-p "yasnippet")
(declare-function yas-exit-snippet "yasnippet")
(declare-function yas--find-next-field-boundary-guard load-file-name t t)

(custom-set-variables
 '(yas-prompt-functions nil)
 '(yas-use-menu nil))

(push '(yas-minor-mode . 12) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'yasnippet
  (setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS")

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
      map)
    "Keymap for `yas-minor-mode' which overrides global overriding maps.")

  (push `(yas-minor-mode . ,overriding-yas-minor-mode-map)
        overriding-reserved-key-map-alist))

(add-hook 'emacs-startup-hook #'yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


(resolve init-yasnippet)
