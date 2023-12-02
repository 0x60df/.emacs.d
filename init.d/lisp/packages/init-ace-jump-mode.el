
;;;; init-ace-jump-mode.el


(premise init)
(premise mode-line)
(premise bindings)
(premise keyboard)
(premise inst-ace-jump-mode)

(eval-when-compile (require 'ace-jump-mode))

(declare-function ace-jump-mode-enable-mark-sync "ace-jump-mode")

(push '(ace-jump-mode . 22) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'ace-jump-mode
  (setq ace-jump-mode-move-keys
        (append (cl-loop for i from ?a to ?z collect i)
                '(?\; ?: ?@ ?, ?. ?/ ?\[ ?\] ?\\)))
  (ace-jump-mode-enable-mark-sync)
  (advice-add 'ace-jump-push-mark
              :around
              (lambda (ace-jump-push-mark &rest args)
                (if (region-active-p)
                    (let ((mark (mark)))
                      (unwind-protect
                          (apply ace-jump-push-mark args )
                        (unless (and (region-active-p) (eql mark (mark)))
                          (push-mark mark t t))))
                  (apply ace-jump-push-mark args )))))

(overriding-set-key (kbd "ESC M-g") #'ace-jump-char-mode)
(overriding-set-key (kbd "M-g M-j") #'ace-jump-mode)
(overriding-set-key (kbd "H-g") #'ace-jump-mode)
(overriding-set-key (kbd "H-SPC") #'ace-jump-char-mode)
(overriding-set-key (kbd "s-SPC") #'ace-jump-char-mode)
(add-hook
 'jis-keys-initialize-functions
 (lambda ()
   (let ((hyper-henkan-key (vector (event-convert-list
                                (append '(hyper) (jis-key 'henkan) nil)))))
     (overriding-set-key hyper-henkan-key #'ace-jump-char-mode)
     (define-key overriding-balance-mode-map hyper-henkan-key #'keyboard-quit))
   (define-key overriding-balance-mode-map (kbd "H-g") #'keyboard-quit)
   (overriding-set-key
    (vector (event-convert-list (append '(super) (jis-key 'muhenkan) nil)))
    #'ace-jump-char-mode)))
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
