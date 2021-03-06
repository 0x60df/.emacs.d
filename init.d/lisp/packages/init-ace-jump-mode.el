
;;;; init-ace-jump-mode.el


(premise init)
(premise mode-line)
(premise bindings)
(premise inst-ace-jump-mode)

(eval-when-compile (require 'ace-jump-mode))

(declare-function ace-jump-mode-enable-mark-sync "ace-jump-mode")

(push '(ace-jump-mode . 22) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'ace-jump-mode
  (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i))
  (ace-jump-mode-enable-mark-sync))

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
            (overriding-set-key (kbd (concat "A-M-" s)) jump-word-func))))
      (vconcat "!\"#$%&'()*+,-./0123456789:;<=>?@"
               "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
(overriding-set-key (kbd "A-<return>") #'ace-jump-line-mode)
(with-eval-after-load 'iso-transl
  (define-key key-translation-map (vector (logior ?\s ?\A-\^@)) nil))
(overriding-set-key (kbd "A-SPC") #'ace-jump-line-mode)


(resolve init-ace-jump-mode)
