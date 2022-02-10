
;;;; init-auto-complete.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)
(premise inst-auto-complete)

(eval-when-compile (require 'auto-complete))

(declare-function ac-expand-string "auto-complete")
(declare-function ac-activate-completing-map "auto-complete")
(declare-function ac-complete "auto-complete")
(declare-function ac-expand-previous "auto-complete")
(declare-function ac-isearch "auto-complete")
(declare-function ac-quick-help "auto-complete")
(declare-function ac-fuzzy-complete "auto-complete")


;;; setup

(with-eval-after-load 'auto-complete
  (ac-config-default)
  (global-auto-complete-mode 0))


;;; patches

(defvar ac-last-prefix nil
  "Temporary storage for `ac-prefix'.
Keep `ac-prefix' before last `ac-expand'.")
(defvar ac-latest-prefix nil
  "Temporary storage for `ac-prefix'.
Keep `ac-prefix' after last `ac-expand'.")

(defun ac-pullback-last-prefix (ac-expand &rest args)
  "Advising `ac-expand' to pullback last prefix.

If inline completion is followed by `ac-expand'
 this
function pullbacks `ac-prefix' which exists before the
inline candidate is completed."
  (cond ((and (null ac-last-prefix)
              (null ac-latest-prefix))
         (cond ((ac-inline-live-p)                     ; entry with inline
                (setq ac-last-prefix ac-prefix)
                (apply ac-expand args)
                (setq ac-latest-prefix ac-prefix))
               (t (apply ac-expand args))))            ; entry without inline
        ((and (stringp ac-last-prefix)
              (stringp ac-latest-prefix))
         (cond ((equal ac-prefix ac-latest-prefix)     ; exit
                (ac-expand-string ac-last-prefix t)
                (apply ac-expand '(nil))
                (apply ac-expand args)
                (ac-activate-completing-map)
                (setq ac-last-prefix 'pullpacked)
                (setq ac-latest-prefix 'pullbacked))
               (t (setq ac-last-prefix ac-prefix)      ; hold
                  (apply ac-expand args)
                  (setq ac-latest-prefix ac-prefix))))
        (t (apply ac-expand args))))                   ; go

(defun ac-cleanup-last-prefix ()
  "Advising function for `ac-cleanup' to set `ac-last-prefix' as nil."
  (setq ac-last-prefix nil)
  (setq ac-latest-prefix nil))

(with-eval-after-load 'auto-complete
  (advice-add 'ac-expand :around #'ac-pullback-last-prefix)
  (advice-add 'ac-cleanup :after #'ac-cleanup-last-prefix))



;;; settings

(custom-set-variables
 `(ac-delay 0.05)
 '(ac-auto-show-menu nil)
 '(ac-ignore-case nil)
 '(ac-quick-help-delay 1.8)
 '(ac-use-menu-map t))



;;; sources

(with-eval-after-load 'auto-complete-config
  (add-hook 'ruby-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
  (add-hook 'js-mode-hook
            (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))
  (add-hook 'org-mode-hook
            (lambda () (setq ac-sources '(ac-source-yasnippet))))
  (add-hook 'html-mode-hook
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



;; utilities

(defun ac-decide ()
  "`ac-complete' without `ac-fallback-command'."
  (interactive)

  (if (and (stringp ac-prefix)
           (stringp ac-latest-prefix)
           (equal ac-prefix ac-latest-prefix))
      (call-interactively #'ac-complete)
    (advice-add 'ac-fallback-command :override #'ignore)
    (call-interactively #'ac-complete)
    (advice-remove 'ac-fallback-command #'ignore)))



;;; mode-line

(push '(auto-complete-mode . 19) mode-line-minor-mode-priority-alist)



;;; bindings

(with-eval-after-load 'auto-complete
  (defvar overriding-ac-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-M-i") #'auto-complete)
      map)
    "Keymap for `auto-complete-mode' which overrides global overriding maps.")

  (push `(auto-complete-mode . 
                             overriding-ac-mode-map)
        overriding-reserved-key-map-alist)

  (define-key ac-completing-map (kbd "<backtab>") #'ac-expand-previous)
  (define-key ac-completing-map (kbd "C-<tab>") #'ac-isearch)
  (define-key ac-completing-map (kbd "C-S-<iso-lefttab>") #'ac-quick-help)
  (define-key ac-completing-map (kbd "H-<tab>") #'ac-fuzzy-complete)
  (define-key ac-completing-map (kbd "RET") #'ac-decide)

  (define-key ac-menu-map (kbd "RET") #'ac-decide))


(resolve init-auto-complete)
