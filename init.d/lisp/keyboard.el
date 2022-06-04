
;;;; keyboard.el


(premise init)
(premise bindings)


(define-key key-translation-map (kbd "<zenkaku-hankaku>") [?\C-\\])
(define-key key-translation-map (kbd "<hiragana-katakana>") [?\C-\\])
(let ((form (lambda (&optional terminal)
              (define-key local-function-key-map (kbd "<henkan>")
                #'event-apply-hyper-modifier)
              (define-key local-function-key-map (kbd "<muhenkan>")
                #'event-apply-super-modifier))))
  (funcall form)
  (add-hook 'after-make-terminal-functions form))

(define-key global-balance-mode-map (kbd "<henkan>")
  (lambda () (interactive) (balance-mode)))
(define-key global-balance-mode-map (kbd "<muhenkan>")
  (lambda () (interactive) (balance-mode)))


(resolve keyboard)
