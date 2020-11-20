
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

(custom-set-variables
 '(yas-prompt-functions nil)
 '(yas-use-menu nil))

(push '(yas-minor-mode . 12) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (defvar overriding-yas-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-'") #'yas-expand)
      map)
    "Keymap for `yas-minor-mode' which overrides global overriding maps.")

  (push `(yas-minor-mode . ,overriding-yas-minor-mode-map)
        overriding-reserved-key-map-alist)

  (setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) " YS"))

(add-hook 'emacs-startup-hook #'yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


(resolve init-yasnippet)
