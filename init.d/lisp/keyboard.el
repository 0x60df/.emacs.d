;;; -*- lexical-binding: t -*-
;;;; keyboard.el


(premise init)
(premise subr)
(premise bindings)

(let ((form (lambda (&optional _)
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
    (if (eq (balance-mode-context) 'balance-weight-mode)
        (unless balance-mode (balance-weight-mode))
      (unless balance-mode (balance-mode)))))
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
    (if (eq (balance-mode-context) 'balance-weight-mode)
        (unless balance-mode (balance-weight-mode))
      (unless balance-mode (balance-mode)))))

(dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (define-key (default-value 'overriding-balance-mode-map) key
    (lambda ()
      (interactive)
      (cond (balance-mode-transient-super
             (balance-mode 0)
             (setq unread-command-events (append (kbd (concat "s-" key)) nil)))
            (balance-mode-transient-hyper
             (setq unread-command-events (append (kbd (concat "H-" key)) nil)))
            (t (setq unread-command-events
                     (append (kbd (concat "C-" key)) nil)))))))

(define-key (default-value 'overriding-balance-weight-mode-map) (kbd "<henkan>")
  (lambda ()
    (interactive)
    (balance-weight-mode 0)
    (balance-mode)))
(define-key (default-value 'overriding-balance-weight-mode-map)
  (kbd "<muhenkan>")
  (lambda ()
    (interactive)
    (balance-weight-mode 0)
    (balance-mode)))
(define-key global-balance-mode-map (kbd "<zenkaku-hankaku>")
  (lambda ()
    (interactive)
    (if (eq (balance-mode-context) 'balance-weight-mode)
        (unless balance-mode (balance-weight-mode))
      (unless balance-mode (balance-mode)))))
(define-key global-balance-mode-map (kbd "<hiragana-katakana>")
  (lambda ()
    (interactive)
    (if (eq (balance-mode-context) 'balance-weight-mode)
        (unless balance-mode (balance-weight-mode))
      (unless balance-mode (balance-mode)))))
(define-key (default-value 'overriding-balance-weight-mode-map)
  (kbd "<zenkaku-hankaku>")
  (lambda ()
    (interactive)
    (balance-weight-mode 0)
    (balance-mode)))
(define-key (default-value 'overriding-balance-weight-mode-map)
  (kbd "<hiragana-katakana>")
  (lambda ()
    (interactive)
    (balance-weight-mode 0)
    (balance-mode)))

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
(balance-mode-add-to-map-alist
 `(quick-input-method-mode . ,overriding-quick-input-method-mode-map))

(overriding-set-key (kbd "ESC A-<zenkaku-hankaku>")
                    #'quick-input-method-mode)
(overriding-set-key (kbd "ESC A-<hiragana-katakana>")
                    #'quick-input-method-mode)

(defun balance-mode-toggle-extended-hyper ()
  "Toggle if extended hyper prefix is enabled."
  (interactive)
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (kbd "<henkan>"))
      (define-key (default-value 'overriding-balance-mode-map)
        (kbd "<henkan>") nil)
    (define-key (default-value 'overriding-balance-mode-map)
      (kbd "<henkan>") #'undefined))
  (dolist (buffer (buffer-list))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (local-variable-p 'overriding-balance-mode-map)
                   (lookup-key overriding-balance-mode-map (kbd "<henkan>")))
              (define-key overriding-balance-mode-map (kbd "<henkan>") nil)
            (define-key overriding-balance-mode-map (kbd "<henkan>")
              #'undefined))))))

(defun balance-mode-toggle-extended-super ()
  "Toggle if extended super prefix is enabled."
  (interactive)
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (kbd "<muhenkan>"))
      (define-key (default-value 'overriding-balance-mode-map)
        (kbd "<muhenkan>") nil)
    (define-key (default-value 'overriding-balance-mode-map)
      (kbd "<muhenkan>") #'undefined))
  (dolist (buffer (buffer-list))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (local-variable-p 'overriding-balance-mode-map)
                   (lookup-key overriding-balance-mode-map (kbd "<muhenkan>")))
              (define-key overriding-balance-mode-map (kbd "<muhenkan>") nil)
            (define-key overriding-balance-mode-map (kbd "<muhenkan>")
              #'undefined))))))

(defun balance-mode-toggle-extended-alt ()
  "Toggle if extended alt prefix is enabled."
  (interactive)
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (kbd "<zenkaku-hankaku>"))
      (define-key (default-value 'overriding-balance-mode-map)
        (kbd "<zenkaku-hankaku>") nil)
    (define-key (default-value 'overriding-balance-mode-map)
      (kbd "<zenkaku-hankaku>") #'undefined))
  (dolist (buffer (buffer-list))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (local-variable-p 'overriding-balance-mode-map)
                   (lookup-key overriding-balance-mode-map
                               (kbd "<zenkaku-hankaku>")))
              (define-key overriding-balance-mode-map
                (kbd "<zenkaku-hankaku>") nil)
            (define-key overriding-balance-mode-map (kbd "<zenkaku-hankaku>")
              #'undefined)))))
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (kbd "<hiragana-katakana>"))
      (define-key (default-value 'overriding-balance-mode-map)
        (kbd "<hiragana-katakana>") nil)
    (define-key (default-value 'overriding-balance-mode-map)
      (kbd "<hiragana-katakana>") #'undefined))
  (dolist (buffer (buffer-list))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (local-variable-p 'overriding-balance-mode-map)
                   (lookup-key overriding-balance-mode-map
                               (kbd "<hiragana-katakana>")))
              (define-key overriding-balance-mode-map
                (kbd "<hiragana-katakana>") nil)
            (define-key overriding-balance-mode-map
              (kbd "<hiragana-katakana>") #'undefined))))))

(define-key global-balance-mode-map
  (kbd "ESC ESC H-<henkan>") #'balance-mode-toggle-extended-hyper)
(define-key global-balance-mode-map
  (kbd "ESC ESC s-<muhenkan>") #'balance-mode-toggle-extended-super)
(define-key global-balance-mode-map
  (kbd "ESC ESC A-<zenkaku-hankaku>") #'balance-mode-toggle-extended-alt)
(define-key global-balance-mode-map
  (kbd "ESC ESC A-<hiragana-katakana>") #'balance-mode-toggle-extended-alt)


(resolve keyboard)
