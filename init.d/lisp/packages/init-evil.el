
;;;; init-evil.el



;;; base

(premise init)
(premise subr)
(premise inst-evil)


;; setting
(eval-after-load 'evil-vars
  '(custom-set-variables '(evil-default-state 'emacs)
                         '(evil-insert-state-modes nil)
                         '(evil-motion-state-modes nil)
                         '(evil-mode-line-format 'after)))

;; toggle
(defun vi ()
  (interactive)
  (evil-exit-emacs-state))
(evil-ex-define-cmd "q[uit]" 'evil-emacs-state)
(evil-define-command evil-save-and-emacs-state (file &optional bang)
  "Saves the current buffer and toggle to emacs state."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-emacs-state))
(evil-ex-define-cmd "wq" 'evil-save-and-emacs-state)


(define-key evil-motion-state-map (kbd "H-e") #'evil-emacs-state)
(define-key evil-insert-state-map (kbd "H-e") #'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "H-e") #'evil-exit-emacs-state)

(evil-set-toggle-key "C-]")
(global-set-key (kbd "C-z") #'abort-recursive-edit)

;; leader
(defun evil-leader (command)
  (interactive (list (read-from-minibuffer
                      "\\"
                      nil
                      evil-ex-completion-map)))
  (cond ((equal command "q") (evil-quit))
        (t nil)))

(define-key evil-motion-state-map (kbd "\\") #'evil-leader)


;;; tag

;; face for tag
(defface evil-mode-line-tag-face '((t))
  "Face for evil-mode-line-tag."
  :group 'evil)

;; tag string
(setq evil-emacs-state-tag "")
(mapc
 (lambda (state)
   (let* ((toggle (intern (format "evil-%s-state" state)))
          (tag (intern (format "%s-tag" toggle)))
          (face (intern (format "%s-face" tag))))
     (eval `(defface ,face '((t :inherit evil-mode-line-tag-face))
              (format "Face for %s." tag)
              :group 'evil))
     (set tag (concat (propertize "V:" 'face 'evil-mode-line-tag-face)
                      (eval `(propertize (replace-regexp-in-string
                                          "^ <\\(.\\)> $" "\\1" ,tag)
                                         'face ',face))
                      (propertize " " 'face 'evil-mode-line-tag-face)))))
 '(normal insert visual replace operator motion))

;; patch for mouse
(defadvice evil-generate-mode-line-tag (after evil-generate-dry-mode-line-tag)
  (setq ad-return-value (evil-state-property state :tag t)))

(ad-activate 'evil-generate-mode-line-tag)


;;; cursor


;; prohibit refresh on some condition
(defcustom evil-reasons-for-interruption-of-reflesh-cursor nil
  ""
  :type '(repeat sexp)
  :group 'evil
  :set '(lambda (symbol value)
          (set-default symbol
                       (apply 'append (mapcar (lambda (l) (eval (car (cdr l))))
                                              (get symbol 'theme-value))))))
(defadvice evil-refresh-cursor (around interrupt-refresh-cursor)
  (letrec ((any (lambda (l)
                  (cond ((null l) l)
                        ((eval (car l)) (eval (car l)))
                        (t (funcall any (cdr l)))))))
    (unless (funcall any evil-reasons-for-interruption-of-reflesh-cursor)
      ad-do-it)))

(ad-activate 'evil-refresh-cursor)

;; generate cursor before refresh-cursor
(defcustom evil-emacs-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-normal-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-insert-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-visual-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-replace-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-operator-state-cursor-adjuster nil "" :group 'evil)
(defcustom evil-motion-state-cursor-adjuster nil "" :group 'evil)
(call-with-runtime-bindings
 ((evil-emacs-state-cursor evil-emacs-state-cursor-adjuster)
  (evil-normal-state-cursor evil-normal-state-cursor-adjuster)
  (evil-insert-state-cursor evil-insert-state-cursor-adjuster)
  (evil-visual-state-cursor evil-visual-state-cursor-adjuster)
  (evil-replace-state-cursor evil-replace-state-cursor-adjuster)
  (evil-operator-state-cursor evil-operator-state-cursor-adjuster)
  (evil-motion-state-cursor evil-motion-state-cursor-adjuster))
 evil-refresh-cursor bind-cursor-variables)


;;; retrieve key

(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map (kbd "C-.") 'nil)))


;;; start

(evil-mode 1)

;; undo-tree
(eval-after-load 'undo-tree
  '(custom-set-variables '(undo-tree-mode-lighter " UT")))
(global-undo-tree-mode -1)


(resolve init-evil)
