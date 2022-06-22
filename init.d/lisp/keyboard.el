;;; -*- lexical-binding: t -*-
;;;; keyboard.el


(premise init)
(premise subr)
(premise bindings)

(defvar jis-keys (list (kbd "<zenkaku-hankaku>")
                       (kbd "<muhenkan>")
                       (kbd "<henkan>")
                       (kbd "<hiragana-katakana>")
                       nil)
  "The list of keys for JIS keyboard.
Each entry represents hankaku/zenkaku, muhenkan, henkan,
hiragana/katakana and eisu keys.
`jis-keys--initialize' initializes JIS keys by using the
value of this variable.  Because `jis-keys--initialize' is
called by `after-init-hook', modification of this variable
during loading init files take effect.")

(defcustom jis-keys-initialize-functions nil
  "List of function who is called with `jis-keys--initialize'.
Because `jis-keys--initialize' is called by `after-init-hook',
adding functions to this user option is valid only during
initialization."
  :group 'user
  :type 'hook)

(defun jis-key (desc)
  "Return key sequence described as DESC.
Valid value of DESC are the symbols: hankaku/zenkaku,
muhenkan, henkan, katakana/hiragana, and eisu.
If DESC is other than above, return nil."
  (cond ((eq desc 'hankaku/zenkaku) (car jis-keys))
        ((eq desc 'muhenkan) (cadr jis-keys))
        ((eq desc 'henkan) (caddr jis-keys))
        ((eq desc 'katakana/hiragana) (cadddr jis-keys))
        ((eq desc 'eisu) (car (cddddr jis-keys)))))

(defun jis-keys--initialize ()
  "Initialize JIS keys by using `jis-keys'.
This function is intended to be called once at
initialization, by adding this to `after-init-hook'.
At the end of this function,
`jis-keys-initialize-functions' are called."
  (let ((form (lambda (&optional _)
                (define-key local-function-key-map (jis-key 'hankaku/zenkaku)
                  #'event-apply-alt-modifier)
                (define-key local-function-key-map (jis-key 'katakana/hiragana)
                  #'event-apply-alt-modifier)
                (define-key local-function-key-map (jis-key 'henkan)
                  #'event-apply-hyper-modifier)
                (define-key local-function-key-map (jis-key 'muhenkan)
                  #'event-apply-super-modifier))))
    (funcall form)
    (add-hook 'after-make-terminal-functions form))
  (run-hooks 'jis-keys-initialize-functions))

(add-hook 'after-init-hook #'jis-keys--initialize)



(define-minor-mode quick-input-method-mode
  "Enable key binding for `toggle-input-method'."
  :group 'user
  :keymap)

(add-hook
 'jis-keys-initialize-functions
 (lambda ()
   (defvar overriding-quick-input-method-mode-map
     (let ((map (make-sparse-keymap)))
       (define-key map (jis-key 'hankaku/zenkaku) #'toggle-input-method)
       (define-key map (jis-key 'katakana/hiragana) #'toggle-input-method)
       map)
     "Overriding keymap for `quick-input-method-mode'.")

   (push `(quick-input-method-mode . ,overriding-quick-input-method-mode-map)
         overriding-reserved-key-map-alist)
   (balance-mode-add-to-map-alist
    `(quick-input-method-mode . ,overriding-quick-input-method-mode-map))

   (overriding-set-key
    (vector ?\e (event-convert-list
                 (append '(alt) (jis-key 'hankaku/zenkaku) nil)))
    #'quick-input-method-mode)
   (overriding-set-key
    (vector ?\e (event-convert-list
                 (append '(alt) (jis-key 'katakana/hiragana) nil)))
    #'quick-input-method-mode)))



(defvar balance-mode-transient-hyper nil
  "Flag if hyper modifier is activated transiently for `balance-mode'.")
(defvar balance-mode-transient-super nil
  "Flag if super modifier is activated transiently for `balance-mode'.")

(add-hook
 'jis-keys-initialize-functions
 (lambda ()
   (define-key global-balance-mode-map (jis-key 'henkan)
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
   (define-key global-balance-mode-map (jis-key 'muhenkan)
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
                (setq unread-command-events
                      (append (kbd (concat "s-" key)) nil)))
               (balance-mode-transient-hyper
                (setq unread-command-events
                      (append (kbd (concat "H-" key)) nil)))
               (t (setq unread-command-events
                        (append (kbd (concat "C-" key)) nil)))))))

   (define-key (default-value 'overriding-balance-weight-mode-map)
     (jis-key 'henkan)
     (lambda ()
       (interactive)
       (balance-weight-mode 0)
       (balance-mode)))
   (define-key (default-value 'overriding-balance-weight-mode-map)
     (jis-key 'muhenkan)
     (lambda ()
       (interactive)
       (balance-weight-mode 0)
       (balance-mode)))
   (define-key global-balance-mode-map (jis-key 'hankaku/zenkaku)
     (lambda ()
       (interactive)
       (if (eq (balance-mode-context) 'balance-weight-mode)
           (unless balance-mode (balance-weight-mode))
         (unless balance-mode (balance-mode)))))
   (define-key global-balance-mode-map (jis-key 'katakana/hiragana)
     (lambda ()
       (interactive)
       (if (eq (balance-mode-context) 'balance-weight-mode)
           (unless balance-mode (balance-weight-mode))
         (unless balance-mode (balance-mode)))))
   (define-key (default-value 'overriding-balance-weight-mode-map)
     (jis-key 'hankaku/zenkaku)
     (lambda ()
       (interactive)
       (balance-weight-mode 0)
       (balance-mode)))
   (define-key (default-value 'overriding-balance-weight-mode-map)
     (jis-key 'katakana/hiragana)
     (lambda ()
       (interactive)
       (balance-weight-mode 0)
       (balance-mode)))))



(defun balance-mode-toggle-extended-hyper ()
  "Toggle if extended hyper prefix is enabled."
  (interactive)
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (jis-key 'henkan))
      (progn
        (define-key (default-value 'overriding-balance-mode-map)
          (jis-key 'henkan) nil)
        (dolist (buffer (buffer-list))
          (if (buffer-live-p buffer)
              (with-current-buffer buffer
                (if (local-variable-p 'overriding-balance-mode-map)
                    (define-key overriding-balance-mode-map
                      (jis-key 'henkan) nil)))))
        (message "Extended hyper is disabled on balance-mode"))
    (define-key (default-value 'overriding-balance-mode-map)
      (jis-key 'henkan) #'undefined)
    (dolist (buffer (buffer-list))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (if (local-variable-p 'overriding-balance-mode-map)
                (define-key overriding-balance-mode-map
                  (jis-key 'henkan) #'undefined)))))
    (message "Extended hyper is enabled on balance-mode")))

(defun balance-mode-toggle-extended-super ()
  "Toggle if extended super prefix is enabled."
  (interactive)
  (if (lookup-key (default-value 'overriding-balance-mode-map)
                  (jis-key 'muhenkan))
      (progn
        (define-key (default-value 'overriding-balance-mode-map)
          (jis-key 'muhenkan) nil)
        (dolist (buffer (buffer-list))
          (if (buffer-live-p buffer)
              (with-current-buffer buffer
                (if (local-variable-p 'overriding-balance-mode-map)
                    (define-key overriding-balance-mode-map
                      (jis-key 'muhenkan) nil)))))
        (message "Extended super is disabled on balance-mode"))
    (define-key (default-value 'overriding-balance-mode-map)
      (jis-key 'muhenkan) #'undefined)
    (dolist (buffer (buffer-list))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (if (local-variable-p 'overriding-balance-mode-map)
                (define-key overriding-balance-mode-map (jis-key 'muhenkan)
                  #'undefined)))))
    (message "Extended super is enabled on balance-mode")))

(defun balance-mode-toggle-extended-alt ()
  "Toggle if extended alt prefix is enabled."
  (interactive)
  (if (or (lookup-key (default-value 'overriding-balance-mode-map)
                      (jis-key 'hankaku/zenkaku))
          (lookup-key (default-value 'overriding-balance-mode-map)
                      (jis-key 'katakana/hiragana)))
      (progn
        (define-key (default-value 'overriding-balance-mode-map)
          (jis-key 'hankaku/zenkaku) nil)
        (define-key (default-value 'overriding-balance-mode-map)
          (jis-key 'katakana/hiragana) nil)
        (dolist (buffer (buffer-list))
          (if (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (local-variable-p 'overriding-balance-mode-map)
                  (define-key overriding-balance-mode-map
                    (jis-key 'hankaku/zenkaku) nil)
                  (define-key overriding-balance-mode-map
                    (jis-key 'katakana/hiragana) nil)))))
        (message "Extended alt is disabled on balance-mode"))
    (define-key (default-value 'overriding-balance-mode-map)
      (jis-key 'hankaku/zenkaku) #'undefined)
    (define-key (default-value 'overriding-balance-mode-map)
      (jis-key 'katakana/hiragana) #'undefined)
    (dolist (buffer (buffer-list))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (local-variable-p 'overriding-balance-mode-map)
              (define-key overriding-balance-mode-map
                (jis-key 'hankaku/zenkaku) #'undefined)
              (define-key overriding-balance-mode-map
                (jis-key 'katakana/hiragana) #'undefined)))))
    (message "Extended alt is enabled on balance-mode")))

(add-hook
 'jis-keys-initialize-functions
 (lambda ()
   (define-key global-balance-mode-map
     (vector
      ?\e ?\e
      (event-convert-list (append '(hyper) (jis-key 'henkan) nil)))
     #'balance-mode-toggle-extended-hyper)
   (define-key global-balance-mode-map
     (vector
      ?\e ?\e
      (event-convert-list (append '(super) (jis-key 'muhenkan) nil)))
     #'balance-mode-toggle-extended-super)
   (define-key global-balance-mode-map
     (vector
      ?\e ?\e
      (event-convert-list (append '(alt) (jis-key 'hankaku/zenkaku) nil)))
     #'balance-mode-toggle-extended-alt)
   (define-key global-balance-mode-map
     (vector
      ?\e ?\e
      (event-convert-list (append '(alt) (jis-key 'katakana/hiragana) nil)))
     #'balance-mode-toggle-extended-alt)))


(resolve keyboard)
