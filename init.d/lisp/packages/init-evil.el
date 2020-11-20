
;;;; init-evil.el


(premise init)
(premise custom)
(premise mode-line)
(premise theme)
(premise bindings)
(premise inst-evil)

(eval-when-compile (require 'evil))

(declare-function evil-exit-emacs-state "evil-commands" t t)
(declare-function evil-emacs-state "evil-states" t t)
(declare-function evil-set-toggle-key "evil-vars")
(declare-function evil-ex-p "evil-ex")
(declare-function evil-ex-file-arg "evil-ex")
(declare-function evil-write "evil-commands" t t)
(declare-function evil-set-command-properties "evil-common")
(declare-function evil-ex-define-cmd "evil-ex")

(declare-function evil-save-and-emacs-state load-file-name t t)


;;; settings

;; base
(custom-set-variables
 '(evil-default-state 'emacs)
 '(evil-insert-state-modes nil)
 '(evil-motion-state-modes nil)
 '(evil-mode-line-format 'after))

;; undo-tree
(custom-set-variables
 '(undo-tree-mode-lighter " UT"))
(with-eval-after-load 'undo-tree
  (global-undo-tree-mode 0))



;;; mode-line

(defface evil-normal-state-tag
  '((t))
  "Face for evil-normal-state-tag."
  :group 'user)

(defface evil-insert-state-tag
  '((t))
  "Face for evil-insert-state-tag."
  :group 'user)

(defface evil-replace-state-tag
  '((t))
  "Face for evil-replace-state-tag."
  :group 'user)

(defface evil-operator-state-tag
  '((t))
  "Face for evil-operator-state-tag."
  :group 'user)

(defface evil-visual-state-tag
  '((t))
  "Face for evil-visual-state-tag."
  :group 'user)

(defface evil-motion-state-tag
  '((t))
  "Face for evil-motion-state-tag."
  :group 'user)

(defun evil-with-mode-line-format-raw (evil-refresh-mode-line &rest args)
  "Advising `evil-refresh-mode-line' to work with custom mode-line.
Call `evil-refresh-mode-line' with local `mode-line-format'
as `mode-line-format-raw'.
After `evil-refresh-mode-line', set default value of
`mode-line-format' by local value with
`mode-line-format-auto-truncate'."
  (let ((mode-line-format mode-line-format-raw))
    (apply evil-refresh-mode-line args)))

(with-eval-after-load 'evil
  (setq evil-emacs-state-tag "")

  (mapc
   (lambda (state)
     (let* ((toggle (intern (format "evil-%s-state" state)))
            (tag (intern (format "%s-tag" toggle))))
       (set tag (concat "V:"
                        (propertize (replace-regexp-in-string
                                     "^ <\\(.\\)> $" "\\1" (symbol-value tag))
                                    'face tag)
                        " "))))
   '(normal insert replace operator motion))

  (mapc
   (lambda (selection)
     (let ((tag (intern (format "evil-visual-%s-tag" selection))))
       (set tag (concat "V:"
                        (propertize (replace-regexp-in-string
                                     "^ <\\(.+\\)> $" "\\1" (symbol-value tag))
                                    'face 'evil-visual-state-tag)
                        " "))))
   '(char line screen-line block))

  (advice-add 'evil-refresh-mode-line :around #'evil-with-mode-line-format-raw))



;;; cursor

(defcustom evil-refresh-cursor-interrupt-conditions nil
  "List of condition forms to interrupt `evil-refresh-cursor'."
  :type '(repeat sexp)
  :group 'user)

(defun evil-interrupt-refresh-cursor (evil-refresh-cursor &rest args)
  "Advising `evil-refresh-cursor' to interrupt on specific condition.
Conditions are specified by `evil-refresh-cursor-interrupt-conditions'."
  (letrec ((any (lambda (l)
                  (cond ((null l) l)
                        ((eval (car l)) (car l))
                        (t (funcall any (cdr l)))))))
    (unless (funcall any evil-refresh-cursor-interrupt-conditions)
      (apply evil-refresh-cursor args ))))

(with-eval-after-load 'evil
  (advice-add 'evil-refresh-cursor :around #'evil-interrupt-refresh-cursor)
  (with-eval-after-load 'auto-complete
    (add-to-list 'evil-refresh-cursor-interrupt-conditions
                  '(and (memq evil-state '(emacs insert))
                        (ac-menu-live-p)))))



;;; theme support

(with-eval-after-load 'evil-states
  (mapc (lambda (state)
          (let ((cursor (intern (format "evil-%s-state-cursor" state))))
            (catch-up-theme-value cursor)))
        '(emacs normal insert replace operator visual motion)))



;;; bindings

(with-eval-after-load 'evil-states
  (define-key evil-normal-state-map (kbd "C-.") nil))



;;; entry and exit

(defun evil-mode-and-exit-emacs-state ()
  "Enable `evil-mode' and `evil-exit-emacs-state'."
  (interactive)
  (evil-mode)
  (evil-exit-emacs-state))

(defalias 'vi 'evil-mode-and-exit-emacs-state "Enter-evil.")

(overriding-set-key (kbd "H-e") #'evil-mode-and-exit-emacs-state)
(overriding-set-key (kbd "C-c DEL") #'evil-mode-and-exit-emacs-state)

(with-eval-after-load 'evil
  (add-hook 'evil-mode-hook
            (lambda ()
              (if evil-mode
                  (fset 'vi 'evil-exit-emacs-state)
                (fset 'vi 'evil-mode-and-exit-emacs-state))))

  (define-key evil-motion-state-map (kbd "H-e") #'evil-emacs-state)
  (define-key evil-insert-state-map (kbd "H-e") #'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "H-e") #'evil-exit-emacs-state)

  (evil-set-toggle-key "C-c DEL")

  (evil-define-command evil-save-and-emacs-state (file &optional bang)
    "Saves the current buffer and toggle to emacs state."
    :repeat nil
    (interactive "<f><!>")
    (evil-write nil nil nil file bang)
    (evil-emacs-state))

  (evil-ex-define-cmd "q[uit]" #'evil-emacs-state)
  (evil-ex-define-cmd "wq" #'evil-save-and-emacs-state)

  (evil-mode))


(resolve init-evil)
