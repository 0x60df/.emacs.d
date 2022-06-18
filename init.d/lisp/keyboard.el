;;; -*- lexical-binding: t -*-
;;;; keyboard.el


(premise init)
(premise bindings)
(premise subr)

(let ((form (lambda (&optional terminal)
              (define-key local-function-key-map (kbd "<zenkaku-hankaku>")
                #'event-apply-alt-modifier)
              (define-key local-function-key-map (kbd "<hiragana-katakana>")
                #'event-apply-alt-modifier)
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

(define-minor-mode quick-input-method-mode
  "Enable key binding for `toggle-input-method'."
  :group 'user
  :keymap)

(defvar overriding-quick-input-method-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<zenkaku-hankaku>") #'toggle-input-method)
    (define-key map (kbd "<hiragana-katakana>") #'toggle-input-method)
    map)
  "Overriding keymap for `quick-input-method-mode'.")

(push `(quick-input-method-mode . ,overriding-quick-input-method-mode-map)
      overriding-reserved-key-map-alist)

(overriding-set-key (kbd "ESC ESC A-<zenkaku-hankaku>")
                    #'quick-input-method-mode)
(overriding-set-key (kbd "ESC ESC A-<hiragana-katakana>")
                    #'quick-input-method-mode)


(resolve keyboard)
