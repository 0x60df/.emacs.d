
;;;; init-auto-complete.el



;;; base

(premise init)
(premise subr)
(premise inst-auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(eval-after-load 'auto-complete
  '(custom-set-variables '(ac-auto-show-menu nil)
                         '(ac-ignore-case nil)
                         '(ac-quick-help-delay 1.8)
                         '(ac-use-menu-map t)))


;;; bindings

(global-set-key (kbd "H-<tab>") 'ac-fuzzy-complete)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)
(define-key ac-completing-map (kbd "C-<tab>") 'ac-isearch)
(define-key ac-completing-map (kbd "C-S-<iso-lefttab>") 'ac-quick-help)
(define-key ac-completing-map (kbd "H-<tab>") 'ac-fuzzy-complete)
(define-key ac-menu-map (kbd "<tab>") 'ac-next)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)


;;; source

(add-hook 'ruby-mode-hook
          (lambda () (setq ac-sources (append '(ac-source-yasnippet)
                                              ac-sources))))

;;; patch

(defvar ac-last-prefix nil)

(defadvice ac-expand (around ac-pullback-on-second-expand)
  (cond ((not (eq last-command 'ac-expand)) ;fist expand
         (setq ac-last-prefix ac-prefix)
         ad-do-it)
        (ac-last-prefix                 ;second expand
         (ac-expand-string ac-last-prefix (eq last-command 'ac-expand))
         (if (and ac-candidates (< 1 (length ac-candidates))) (ac-next))
         (setq ac-last-prefix nil))
        (t ad-do-it)))                  ;from third expand
(ad-activate 'ac-expand)

(defadvice ac-cleanup (after ac-cleanup-last-prefix)
  (setq ac-last-prefix nil))
(ad-activate 'ac-cleanup)


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


;;; cursor

(defcustom popup-isearch-cursor-color-adjuster nil "" :group 'auto-complete)
(call-with-runtime-bindings
 ((popup-isearch-cursor-color popup-isearch-cursor-color-adjuster))
 popup-isearch bind-cursor-variable)
(call-with-runtime-bindings
 ((popup-isearch-cursor-color popup-isearch-cursor-color-adjuster))
 popup-menu* bind-cursor-variable)

(defcustom ac-fuzzy-cursor-color-adjuster nil "" :group 'auto-complete)
(call-with-runtime-bindings
 ((ac-fuzzy-cursor-color ac-fuzzy-cursor-color-adjuster))
 ac-fuzzy-complete bind-cursor-variable)
(call-with-runtime-bindings
 ((ac-fuzzy-cursor-color ac-fuzzy-cursor-color-adjuster))
 ac-start bind-cursor-variable)


(resolve init-auto-complete)
