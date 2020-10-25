
;;;; init-ace-jump-mode.el


(premise init)
(premise inst-ace-jump-mode)

(global-set-key (kbd "M-g M-j") 'ace-jump-mode)
(global-set-key (kbd "H-g") 'ace-jump-mode)
(mapc (lambda (c)
        (eval-after-load 'iso-transl
          `(define-key key-translation-map (vector (logior ,c ?\A-\^@)) nil))
        (global-set-key
         (kbd (concat "A-" (char-to-string c)))
         `(lambda ()
            (interactive)
            (funcall 'ace-jump-char-mode ,c)))
        (global-set-key
         (kbd (concat "A-M-" (char-to-string c)))
         `(lambda ()
            (interactive)
            (funcall 'ace-jump-word-mode ,c))))
      (vconcat "!\"#$%&'()*+,-./0123456789:;<=>?@"
               "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
(global-set-key (read-kbd-macro "A-<return>") #'ace-jump-line-mode)


(resolve init-ace-jump-mode)
