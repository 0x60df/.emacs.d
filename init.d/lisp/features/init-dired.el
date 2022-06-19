
;;;; init-dired.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always))

(add-hook 'balance-mode-update-keys-hook
          (lambda ()
            (when (eq major-mode #'dired-mode)
              (balance-mode-implement-keys
               (list (kbd "C-c s g")
                     (kbd "C-c s l")
                     (kbd "C-c s r")
                     (kbd "C-c s z")
                     (kbd "C-c s f")
                     (kbd "C-c s o"))
               overriding-balance-weight-mode-map)
              (balance-mode-alias-keys
               (list `(,(kbd "c SPC s g") . ,(kbd "c s g"))
                     `(,(kbd "c SPC s l") . ,(kbd "c s l"))
                     `(,(kbd "c SPC s r") . ,(kbd "c s r"))
                     `(,(kbd "c SPC s z") . ,(kbd "c s z"))
                     `(,(kbd "c SPC s f") . ,(kbd "c s f"))
                     `(,(kbd "c SPC s o") . ,(kbd "c s o")))
               overriding-balance-weight-mode-map))))


(resolve init-dired)
