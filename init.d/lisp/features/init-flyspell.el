
;;;; init-flyspell.el


(premise init)
(premise mode-line)
(premise bindings)

(eval-when-compile (require 'flyspell))

(declare-function flyspell-goto-next-error "flyspell")
(declare-function flyspell-auto-correct-word "flyspell")
(declare-function flyspell-auto-correct-previous-word "flyspell")

(custom-set-variables
 '(flyspell-mode-line-string " FlyS"))

(push '(flyspell-mode . 43) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-c $") #'flyspell-mode)
(overriding-set-key (kbd "H-4") #'flyspell-mode)

(with-eval-after-load 'flyspell
  (defvar overriding-flyspell-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c $") (make-sparse-keymap))
      (define-key map (kbd "C-c $ $") #'flyspell-mode)

      (define-key map (kbd "C-c $ >") #'flyspell-goto-next-error)
      (define-key map (kbd "C-c $ .") #'flyspell-auto-correct-word)
      (define-key map (kbd "C-c $ ,") #'flyspell-goto-next-error)
      (define-key map (kbd "C-c $ ;") #'flyspell-auto-correct-previous-word)
      map)
    "Keymap for `flyspell-mode' which overrides global overriding maps.")

  (push `(flyspell-mode . ,overriding-flyspell-mode-map)
        overriding-reserved-key-map-alist))


(resolve init-flyspell)
