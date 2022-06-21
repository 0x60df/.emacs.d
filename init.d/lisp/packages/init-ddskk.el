
;;;; init-ddskk.el


(premise init)
(premise mode-line)
(premise bindings)
(premise inst-ddskk)

(declare-function skk-henkan-inactivate "skk")
(declare-function skk-henkan-off-by-quit "skk")
(declare-function skk-erase-prefix "skk-macs")

(declare-function ccc-update-buffer-local-cursor-color-suppressor
                  load-file-name t t)
(declare-function evil-refresh-cursor-suppressor load-file-name t t)


;;; setup

(with-eval-after-load 'skk
  (ccc-setup))



;;; settings

(custom-set-variables
 '(skk-isearch-mode-enable nil)
 '(skk-byte-compile-init-file t)
 '(skk-user-directory (concat user-emacs-directory "ddskk/"))
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
 '(skk-status-indicator 'minor-mode)
 '(skk-compare-jisyo-size-when-saving nil)
 '(skk-latin-mode-string (propertize "SKK" 'face 'mode-line-emphasis))
 '(skk-hiragana-mode-string (propertize "かな" 'face 'mode-line-emphasis))
 '(skk-katakana-mode-string (propertize "カナ" 'face 'mode-line-emphasis))
 '(skk-jisx0208-latin-mode-string (propertize "全英" 'face 'mode-line-emphasis))
 '(skk-abbrev-mode-string (propertize "aあ" 'face 'mode-line-emphasis))
 '(skk-jisx0201-mode-string (propertize "ｶﾀｶﾅ" 'face 'mode-line-emphasis))
 '(skk-dcomp-activate t)
 '(skk-show-annotation t)
 '(skk-show-inline nil)
 '(skk-inline-show-face nil)
 '(skk-show-candidates-always-pop-to-buffer t))

(add-hook 'jis-keys-initialize-functions
          (lambda ()
            (custom-set-variables
             '(skk-sticky-key (jis-key 'henkan))
             '(skk-kakutei-key (jis-key 'muhenkan)))))



;;; mode-line

(push '(skk-mode . 0) mode-line-minor-mode-priority-alist)



;;; bindings

(overriding-set-key (kbd "s-\\") #'skk-mode)
(add-hook 'jis-keys-initialize-functions
          (lambda ()
            (overriding-set-key
             (vector (event-convert-list
                      (append '(control) (jis-key 'hankaku/zenkaku) nil)))
             #'skk-mode)
            (overriding-set-key
             (vector (event-convert-list
                      (append '(control) (jis-key 'katakana/hiragana) nil)))
             #'skk-mode)))



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
