
;;;; keyboard.el


(premise init)


(define-key key-translation-map (kbd "<zenkaku-hankaku>") [?\C-\\])
(define-key key-translation-map (kbd "<hiragana-katakana>") [?\C-\\])
(let ((form (lambda (&optional terminal)
              (define-key local-function-key-map (kbd "<henkan>")
                #'event-apply-hyper-modifier)
              (define-key local-function-key-map (kbd "<muhenkan>")
                #'event-apply-super-modifier))))
  (funcall form)
  (add-hook 'after-make-terminal-functions form))


(resolve keyboard)
