
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

(push '(flyspell-mode . 42) mode-line-minor-mode-priority-alist)

(overriding-set-key (kbd "C-c $") #'flyspell-mode)
(overriding-set-key (kbd "C-l 4") #'flyspell-mode)

(add-to-list 'balance-mode-key-list (kbd "C-l 4"))
(add-to-list 'balance-mode-key-alias-alist `(,(kbd "l SPC 4") . ,(kbd "l 4")))

(defvar-local balance-mode-flyspell nil
  "`flyspell-mode' but t if `balance-mode' is active.")

(with-eval-after-load 'flyspell
  (defvar overriding-flyspell-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c $") (make-sparse-keymap))
      (define-key map (kbd "C-c $ $") #'flyspell-mode)

      (define-key map (kbd "C-c $ >") #'flyspell-goto-next-error)
      (define-key map (kbd "C-c $ .") #'flyspell-auto-correct-word)
      (define-key map (kbd "C-c $ ,") #'flyspell-goto-next-error)
      (define-key map (kbd "C-c $ ;") #'flyspell-auto-correct-previous-word)

      (define-key map (kbd "C-l 4") (make-sparse-keymap))
      (define-key map (kbd "C-l 4 4") #'flyspell-mode)

      (define-key map (kbd "C-l 4 >") #'flyspell-goto-next-error)
      (define-key map (kbd "C-l 4 .") #'flyspell-auto-correct-word)
      (define-key map (kbd "C-l 4 ,") #'flyspell-goto-next-error)
      (define-key map (kbd "C-l 4 ;") #'flyspell-auto-correct-previous-word)
      map)
    "Keymap for `flyspell-mode' which overrides global overriding maps.")

  (push `(flyspell-mode . ,overriding-flyspell-mode-map)
        overriding-reserved-key-map-alist)

  (defvar balance-mode-overriding-flyspell-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c $") (make-sparse-keymap))
      (define-key map (kbd "c $ $") #'flyspell-mode)

      (define-key map (kbd "c $ >") #'flyspell-goto-next-error)
      (define-key map (kbd "c $ .") #'flyspell-auto-correct-word)
      (define-key map (kbd "c $ ,") #'flyspell-goto-next-error)
      (define-key map (kbd "c $ ;") #'flyspell-auto-correct-previous-word)

      (define-key map (kbd "l 4") (make-sparse-keymap))
      (define-key map (kbd "l 4 4") #'flyspell-mode)

      (define-key map (kbd "l 4 >") #'flyspell-goto-next-error)
      (define-key map (kbd "l 4 .") #'flyspell-auto-correct-word)
      (define-key map (kbd "l 4 ,") #'flyspell-goto-next-error)
      (define-key map (kbd "l 4 ;") #'flyspell-auto-correct-previous-word)
      map)
    "Keymap for `flyspell-mode' which overrides global overriding maps.")

  (add-hook 'flyspell-mode-hook (lambda ()
                                  (if (and flyspell-mode balance-mode)
                                      (setq balance-mode-flyspell t)
                                    (setq balance-mode-flyspell nil))))
  (add-hook 'balance-mode-hook (lambda ()
                                 (if balance-mode
                                     (if flyspell-mode
                                         (setq balance-mode-flyspell t))
                                   (setq balance-mode-flyspell nil))))

  (balance-mode-add-to-map-alist
   `(balance-mode-flyspell . ,balance-mode-overriding-flyspell-mode-map)))


(resolve init-flyspell)
