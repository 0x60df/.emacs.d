
;;;; init-auto-complete.el



;;; base

(global-auto-complete-mode t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(setq ac-auto-show-menu nil)
(setq ac-ignore-case nil)


;;; bindings

(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "C-<tab>") 'ac-isearch)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "<tab>") 'ac-next)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
(define-key ac-menu-map (kbd "C-<tab>") 'ac-isearch)


;;; source

(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq ac-sources (append '(ac-source-features
                                                ac-source-functions
                                                ac-source-yasnippet
                                                ac-source-variables
                                                ac-source-symbols
                                                )
                                              ac-sources))))

(add-hook 'ruby-mode-hook
          (lambda () (setq ac-sources (append '(ac-source-yasnippet)
                                              ac-sources))))

;;; patch

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu-without-update))
    (popup-next ac-menu)
    (if (eq this-command 'ac-next)
        (setq ac-dwim-enable t))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu-without-update))
    (popup-previous ac-menu)
    (if (eq this-command 'ac-previous)
        (setq ac-dwim-enable t))))

(defun ac-show-menu-without-update ()
  (when (not (eq ac-show-menu t))
    (setq ac-show-menu t)
    (ac-inline-hide)
    (ac-remove-quick-help)
    (if ac-candidates
        (progn
          (setq ac-completing t)
          (ac-activate-completing-map))
      (setq ac-completing nil)
      (ac-deactivate-completing-map))))


;;; faces

(defface ac-dictionary-candidate-face
  '((t :inherit ac-candidate-face))
  "Face for dictionary candidate."
  :group 'auto-complete)

(defface ac-dictionary-selection-face
  '((t :inherit ac-selection-face))
  "Face for dictionary selection."
  :group 'auto-complete)

(defface ac-symbols-candidate-face
  '((t :inherit ac-candidate-face))
  "Face for symbols candidate."
  :group 'auto-complete)

(defface ac-symbols-selection-face
  '((t :inherit ac-selection-face))
  "Face for symbols selection."
  :group 'auto-complete)

(defface ac-variables-candidate-face
  '((t :inherit ac-candidate-face))
  "Face for variables candidate."
  :group 'auto-complete)

(defface ac-variables-selection-face
  '((t :inherit ac-selection-face))
  "Face for variables selection."
  :group 'auto-complete)

(defface ac-functions-candidate-face
  '((t :inherit ac-candidate-face))
  "Face for functions candidate."
  :group 'auto-complete)

(defface ac-functions-selection-face
  '((t :inherit ac-selection-face))
  "Face for functions selection."
  :group 'auto-complete)

(add-to-list 'ac-source-dictionary
             '(candidate-face . ac-dictionary-candidate-face))
(add-to-list 'ac-source-dictionary
             '(selection-face . ac-dictionary-selection-face))
(add-to-list 'ac-source-symbols
             '(candidate-face . ac-symbols-candidate-face))
(add-to-list 'ac-source-symbols
             '(selection-face . ac-symbols-selection-face))
(add-to-list 'ac-source-variables
             '(candidate-face . ac-variables-candidate-face))
(add-to-list 'ac-source-variables
             '(selection-face . ac-variables-selection-face))
(add-to-list 'ac-source-functions
             '(candidate-face . ac-functions-candidate-face))
(add-to-list 'ac-source-functions
             '(selection-face . ac-functions-selection-face))
