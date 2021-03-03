
;;;; smartparens.el


(premise init)
(premise custom)
(premise advice)
(premise mode-line)
(premise bindings)
(premise feature)
(premise inst-smartparens)

(lazy-autoload 'sp-splice-sexp "smartparens")
(lazy-autoload 'sp-rewrap-sexp "smartparens")
(declare-function sp-local-pair "smartparens")

(declare-function sp-show--enclosing-pair-cleanup load-file-name t t)

(custom-set-variables
 '(sp-highlight-pair-overlay nil)
 '(sp-highlight-wrap-overlay t)
 '(sp-highlight-wrap-tag-overlay t)
 '(sp-autodelete-pair nil)
 '(sp-autodelete-wrap nil))

(push '(smartparens-mode . 14) mode-line-minor-mode-priority-alist)

(defvar sp-show-enclosing-pair-level 0 "Level for highlighting enclosing pair.")

(define-minor-mode sp-show-enclosing-pair-mode
  "Minor mode for enhancing `sp-show-enclosing-pair'."
  nil
  nil
  '(("p" . sp-show-enclosing-pair-dwim)
    ("P" . sp-show-enclosing-pair-dwim-reverse)
    ("(" . sp-show-enclosing-pair-jump-backward)
    (")" . sp-show-enclosing-pair-jump-forward)
    ("b" . sp-show-enclosing-pair-jump-backward)
    ("f" . sp-show-enclosing-pair-jump-forward)
    ("@" . sp-show-set-region)
    ([?\s] . sp-show-set-region)))

(defun sp-show-enclosing-pair-dwim (&optional arg)
  "`sp-show-enclosing-pair' with auto upgrading the level."
  (interactive "p")

  (sp-show-enclosing-pair-mode)
  (setq sp-show-enclosing-pair-level (+ arg sp-show-enclosing-pair-level))
  (advice-add-for-once
   'sp-show--pair-enc-function
   :before (lambda (&rest args)
             (advice-add-for-once
              'sp-get-enclosing-sexp
              :around (lambda (function &rest args)
                        (or (funcall
                             function
                             (or args
                                 (if (< 0 sp-show-enclosing-pair-level)
                                     sp-show-enclosing-pair-level
                                   (setq sp-show-enclosing-pair-level 1))))
                            (progn
                              (setq sp-show-enclosing-pair-level 1)
                              (funcall function))))))))

(advice-add-for-once 'sp-show-enclosing-pair-dwim
                     :before (lambda (&rest args) (require 'smartparens)))

(with-eval-after-load 'smartparens
  (defun sp-show-enclosing-pair-dwim-reverse (&optional arg)
  "`sp-show-enclosing-pair-dwim' by reverse direction."
  (interactive "p")
  (sp-show-enclosing-pair-dwim (- arg)))

  (defun sp-show-enclosing-pair-jump-forward ()
    "Jump to the highlighted closing pair."
    (interactive)
    (when sp-show-pair-enc-overlays
      (goto-char (overlay-end (cdr sp-show-pair-enc-overlays)))))

  (defun sp-show-enclosing-pair-jump-backward ()
    "Jump to the lighlighted opening pair."
    (interactive)
    (when sp-show-pair-enc-overlays
      (goto-char (overlay-start (car sp-show-pair-enc-overlays)))))

  (defun sp-show-set-region ()
    "Set region by enclosing."
    (interactive)
    (let ((point (overlay-start (car sp-show-pair-enc-overlays)))
          (mark (overlay-end (cdr sp-show-pair-enc-overlays))))
      (goto-char point)
      (push-mark mark t t)))

  (defun sp-show--enclosing-pair-cleanup (&rest args)
    "Advising function for resetting `sp-show-enclosing-pair-level'"
    (unless (and (memq last-command '(sp-show-enclosing-pair-dwim
                                      sp-show-enclosing-pair-dwim-reverse))
                 (memq this-command '(sp-show-enclosing-pair-dwim
                                      sp-show-enclosing-pair-dwim-reverse
                                      nil)))
      (sp-show-enclosing-pair-mode 0)
      (setq sp-show-enclosing-pair-level 0)))

  (mapc (lambda (function)
          (add-to-list 'sp-show-enclosing-pair-commands function))
        '(sp-show-enclosing-pair-dwim sp-show-enclosing-pair-dwim-reverse))

  (advice-add 'sp-show--pair-delete-enc-overlays
              :after #'sp-show--enclosing-pair-cleanup))

(overriding-set-key (kbd "C-(") #'sp-splice-sexp)
(overriding-set-key (kbd "C-)") #'sp-rewrap-sexp)
(overriding-set-key (kbd "C-l h p") #'sp-show-enclosing-pair-dwim)
(overriding-set-key (kbd "C-l p") #'sp-show-enclosing-pair-dwim)

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'smartparens)

            (mapc (lambda (mode)
                    (sp-local-pair mode "'" nil :actions nil)
                    (sp-local-pair mode "`" "'"
                                   :when '(sp-in-docstring-p
                                           sp-in-string-p
                                           sp-in-comment-p)))
                  '(emacs-lisp-mode lisp-interaction-mode))

            (smartparens-global-mode)
            (show-smartparens-global-mode)))


(resolve init-smartparens)
