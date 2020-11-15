
;;;; init-auto-complete.el


(premise init)
(premise custom)
(premise bindings)
(premise inst-auto-complete)


;;; patches

(defvar ac-last-prefix nil
  "Temporary storage for `ac-prefix'.
Keep `ac-prefix' before last `ac-expand'.")
(defvar ac-latest-prefix nil
  "Temporary storage for `ac-prefix'.
Keep `ac-prefix' after last `ac-expand'.")

(defun ac-pullback-last-prefix (ac-expand &rest args)
  "Advising `ac-expand' to pullback last prefix.

If inline completion is followed by `ac-expand', this
function pullbacks `ac-prefix' which exists before the
inline candidate is completed."
  (cond ((null ac-last-prefix)
         (cond ((ac-inline-live-p)
                (setq ac-last-prefix ac-prefix)
                (apply ac-expand args)
                (setq ac-latest-prefix ac-prefix))
               (t (apply ac-expand args))))
        ((and (stringp ac-last-prefix)
              (stringp ac-latest-prefix))
         (cond ((equal ac-prefix ac-latest-prefix)
                (ac-expand-string ac-last-prefix t)
                (apply ac-expand '(nil))
                (apply ac-expand args)
                (setq ac-last-prefix nil)
                (setq ac-latest-prefix nil))
               (t (setq ac-last-prefix ac-prefix)
                  (apply ac-expand args)
                  (setq ac-latest-prefix ac-prefix))))
        (t (apply ac-expand args))))

(defun ac-cleanup-last-prefix ()
  "Advising function for `ac-cleanup' to set `ac-last-prefix' as nil."
  (setq ac-last-prefix nil)
  (setq ac-latest-prefix nil))

(with-eval-after-load 'auto-complete
  (advice-add 'ac-expand :around #'ac-pullback-last-prefix)
  (advice-add 'ac-cleanup :after #'ac-cleanup-last-prefix))



;;; settings

(custom-set-variables
 '(ac-auto-show-menu nil)
 '(ac-ignore-case nil)
 '(ac-quick-help-delay 1.8)
 '(ac-use-menu-map t))

(with-eval-after-load 'auto-complete
  (add-to-list 'ac-dictionary-directories
               (concat user-emacs-directory "ac-dict")))



;;; sources

(with-eval-after-load 'auto-complete-config
  (add-hook 'ruby-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
  (add-hook 'js-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet))))



;;; faces

(with-eval-after-load 'auto-complete
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
               '(selection-face . ac-functions-selection-face)))



;;; bindings

(with-eval-after-load 'auto-complete
  (defvar overriding-ac-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-M-i") #'auto-complete)
      map)
    "Keymap for `auto-complete-mode' which overrides global overriding maps.")

  (push `(auto-complete-mode . ,overriding-ac-mode-map)
        overriding-reserved-key-map-alist)

  (define-key ac-completing-map (kbd "<backtab>") #'ac-expand-previous)
  (define-key ac-completing-map (kbd "C-<tab>") #'ac-isearch)
  (define-key ac-completing-map (kbd "C-S-<iso-lefttab>") #'ac-quick-help)
  (define-key ac-completing-map (kbd "H-<tab>") #'ac-fuzzy-complete))


(resolve init-auto-complete)
