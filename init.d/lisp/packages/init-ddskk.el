
;;;; init-ddskk.el


(premise init)
(premise mode-line)
(premise bindings)
(premise feature)
(premise inst-ddskk)

(declare-function skk-henkan-inactivate "skk")
(declare-function skk-henkan-off-by-quit "skk")
(declare-function skk-erase-prefix "skk-macs")
(declare-function skk-remove-minibuffer-setup-hook "skk")
(declare-function skk-kakutei "skk")
(declare-function ccc-setup "ccc")

(lazy-autoload 'skk-mode "skk")

(declare-function ccc-update-buffer-local-cursor-color-suppressor
                  load-file-name t t)
(declare-function evil-refresh-cursor-suppressor load-file-name t t)


;;; setup

(with-eval-after-load 'skk
  (require 'skk-autoloads)
  (ccc-setup))



;;; settings

(custom-set-variables
 '(skk-isearch-mode-enable nil)
 '(skk-byte-compile-init-file t)
 '(skk-user-directory (concat user-emacs-directory "ddskk"))
 '(skk-bayesian-history-file (expand-file-name "bayesian" skk-user-directory))
 '(skk-bayesian-corpus-file (expand-file-name "corpus" skk-user-directory))
 '(skk-init-file (expand-file-name "init" skk-user-directory))
 '(skk-jisyo (expand-file-name "jisyo" skk-user-directory))
 '(skk-backup-jisyo (expand-file-name "jisyo.bak" skk-user-directory))
 '(skk-emacs-id-file (expand-file-name "emacs-id" skk-user-directory))
 '(skk-record-file (expand-file-name "record" skk-user-directory))
 '(skk-study-file (expand-file-name "study" skk-user-directory))
 '(skk-study-backup-file (expand-file-name "study.bak" skk-user-directory))
 '(skk-previous-completion-use-backtab t)
 '(skk-egg-like-newline t)
 '(skk-sticky-key (kbd "<henkan>"))
 '(skk-kakutei-key (kbd "<muhenkan>"))
 '(skk-status-indicator 'minor-mode)
 '(skk-compare-jisyo-size-when-saving nil))



;;; mode-line

(push '(skk-mode . 0) mode-line-minor-mode-priority-alist)



;;; bindings

(overriding-set-key (kbd "s-\\") #'skk-mode)
(overriding-set-key (kbd "C-<zenkaku-hankaku>") #'skk-mode)
(overriding-set-key (kbd "C-<hiragana-katakana>") #'skk-mode)



;;; work with evil

(defconst skk-evil-concession-utility-form
  '(progn
     (defun ccc-update-buffer-local-cursor-color-suppressor
        (ccc-update-buffer-local-cursor-color &rest args)
      "Advising `ccc-update-buffer-local-cursor-color' to suppress ccc.

Cursor color update is suppressed if `evil-state' is not
emacs or `skk-mode' is off."
      (if (and (eq evil-state 'emacs) skk-mode)
          (apply ccc-update-buffer-local-cursor-color args)))
     (defun evil-refresh-cursor-suppressor (evil-refresh-cursor &rest args)
       "Advising `evil-refresh-cursor' to suppress `evil-refresh-cursor'

Cursor refresh is suppressed if `evil-state' is emacs and
`skk-mode' is on."
       (if (not (and (eq evil-state 'emacs) skk-mode))
           (apply evil-refresh-cursor args))))
  "Form to define utilities for concession between skk and evil.
This form will be evaluated after loading skk and evil.
These defun are separated to this form in order to
eliminate compiler warnings.")

(with-eval-after-load 'ccc
  (with-eval-after-load 'evil
    (require 'skk)

    (eval skk-evil-concession-utility-form)

    (advice-add 'ccc-update-buffer-local-cursor-color
                :around #'ccc-update-buffer-local-cursor-color-suppressor)
    (advice-add 'evil-refresh-cursor
                :around #'evil-refresh-cursor-suppressor)))


(resolve init-ddskk)
