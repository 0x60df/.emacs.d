
;;;; init-evil.el



;;; base

;; setting
(custom-set-variables '(evil-default-state 'emacs)
                      '(evil-insert-state-modes nil)
                      '(evil-motion-state-modes nil)
                      '(evil-mode-line-format 'after))

;; toggle
(defun vi ()
  (interactive)
  (evil-exit-emacs-state))
(defun evil-leader (command)
  (interactive "S\\")
  (cond ((eq command 'q) (evil-emacs-state))
        (t nil)))
(define-key evil-normal-state-map (kbd "\\") #'evil-leader)
(define-key evil-motion-state-map (kbd "C-]") #'evil-emacs-state)
(define-key evil-insert-state-map (kbd "C-]") #'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-]") #'evil-exit-emacs-state)
(define-key evil-motion-state-map (kbd "C-\\") #'ignore)


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


;;; cursor

(defcustom evil-reasons-for-interruption-of-reflesh-cursor nil
  ""
  :type '(repeat sexp)
  :group 'evil
  :set '(lambda (symbol value)
          (set-default symbol
                       (apply 'append (mapcar (lambda (l) (eval (car (cdr l))))
                                              (get symbol 'theme-value))))))
(defadvice evil-refresh-cursor (around evil-interrupt-refresh-cursor)
  (letrec ((any (lambda (l)
                  (cond ((null l) l)
                        ((eval (car l)) (eval (car l)))
                        (t (funcall any (cdr l)))))))
    (unless (funcall any evil-reasons-for-interruption-of-reflesh-cursor)
      ad-do-it)))
(ad-activate 'evil-refresh-cursor)


;;; retrieve key

(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map (kbd "C-.") 'other-frame)))


;;; start

(evil-mode 1)

;; undo-tree
(custom-set-variables '(undo-tree-mode-lighter " UT"))
(global-undo-tree-mode -1)
