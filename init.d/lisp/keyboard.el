
;;;; keyboard.el


(premise init)


(define-key key-translation-map (kbd "<zenkaku-hankaku>") [?\C-\\])
(define-key key-translation-map (kbd "<hiragana-katakana>") [?\C-\\])
(define-key local-function-key-map (kbd "<henkan>")
  #'event-apply-hyper-modifier)
(define-key local-function-key-map (kbd "<muhenkan>")
  #'event-apply-super-modifier)


(resolve keyboard)
