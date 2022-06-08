;;; -*- lexical-binding: t -*-
;;;; keyboard.el


(premise init)
(premise bindings)
(premise subr)


(define-key key-translation-map (kbd "<zenkaku-hankaku>") [?\C-\\])
(define-key key-translation-map (kbd "<hiragana-katakana>") [?\C-\\])
(let ((form (lambda (&optional terminal)
              (define-key local-function-key-map (kbd "<henkan>")
                #'event-apply-hyper-modifier)
              (define-key local-function-key-map (kbd "<muhenkan>")
                #'event-apply-super-modifier))))
  (funcall form)
  (add-hook 'after-make-terminal-functions form))

(defvar balance-mode-transient-hyper nil
  "Flag if hyper modifier is activated transiently for `balance-mode'.")
(defvar balance-mode-transient-super nil
  "Flag if super modifier is activated transiently for `balance-mode'.")

(define-key global-balance-mode-map (kbd "<henkan>")
  (lambda () (interactive)
    (setq balance-mode-transient-hyper t)
    (add-hook-for-once
     'post-command-hook
     (lambda ()
       (add-hook-for-once
        'post-command-hook
        (lambda () (setq balance-mode-transient-hyper nil)))))
    (balance-mode)))
(define-key global-balance-mode-map (kbd "<muhenkan>")
  (lambda ()
    (interactive)
    (setq balance-mode-transient-super t)
    (add-hook-for-once
     'post-command-hook
     (lambda ()
       (add-hook-for-once
        'post-command-hook
        (lambda () (setq balance-mode-transient-super nil)))))
    (balance-mode)))

(dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (define-key overriding-balance-mode-map key
    (lambda ()
      (interactive)
      (cond (balance-mode-transient-super
             (balance-mode 0)
             (setq unread-command-events (append (kbd (concat "s-" key)) nil)))
            (balance-mode-transient-hyper
             (setq unread-command-events (append (kbd (concat "H-" key)) nil)))
            (t (setq unread-command-events
                     (append (kbd (concat "C-" key)) nil)))))))


(resolve keyboard)
