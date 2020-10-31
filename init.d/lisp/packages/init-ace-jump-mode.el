
;;;; init-ace-jump-mode.el


(premise init)
(premise bindings)
(premise inst-ace-jump-mode)

(overriding-set-key (kbd "M-g M-j") #'ace-jump-mode)
(overriding-set-key (kbd "H-g") #'ace-jump-mode)
(mapc (lambda (c)
        (let ((s (char-to-string c)))
          (eval-after-load 'iso-transl
            `(define-key key-translation-map (vector (logior ,c ?\A-\^@)) nil))

          (let ((jump-char-func (intern (concat "ace-jump-char-" s))))
            (fset jump-char-func
                  `(lambda ()
                     ,(format "Ace jump char to `%s'." s)
                     (interactive)
                     (funcall-interactively 'ace-jump-char-mode ,c)))
            (overriding-set-key (kbd (concat "A-" s)) jump-char-func))

          (let ((jump-word-func (intern (concat "ace-jump-word-" s))))
            (fset jump-word-func
                  `(lambda ()
                     ,(format "Ace jump word to `%s'." s)
                     (interactive)
                     (funcall-interactively 'ace-jump-word-mode ,c)))
            (overriding-set-key (kbd (concat "A-C-" s)) jump-word-func))))
      (vconcat "!\"#$%&'()*+,-./0123456789:;<=>?@"
               "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
(overriding-set-key (read-kbd-macro "A-<return>") #'ace-jump-line-mode)


(resolve init-ace-jump-mode)
