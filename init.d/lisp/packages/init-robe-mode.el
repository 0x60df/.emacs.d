
;;;; init-robe-mode.el



;;; base

(premise init)
(premise inst-robe-mode)

(eval-when-compile
  (require 'ruby-mode)
  (require 'robe))

(autoload 'robe-start "robe"
  "Start Robe server if it isn't already running.
When called with a prefix argument, kills the current Ruby
process, if any, and starts a new console for the current
project."
  t)


;;; patch

(defadvice ac-robe-candidates (after delete-duplicated-candidates)
  (setq ad-return-value (ac-delete-duplicated-candidates ad-return-value)))

(ad-activate 'ac-robe-candidates)


;;; functions

(defun robe-boot ()
  (interactive)
  (ac-robe-setup)
  (robe-mode 1)
  (save-window-excursion (inf-ruby))
  (robe-start))


;;; bindings

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-cca" 'robe-boot)))

(eval-after-load 'robe
  '(progn
     (define-key robe-mode-map "\M-." 'find-tag)))


;;; faces

(eval-after-load 'auto-complete
  '(progn
     (defface ac-robe-candidate-face
       '((t :inherit ac-candidate-face
            :foreground "#CC342D"))
       "Face for robe candidate."
       :group 'auto-complete)
     (defface ac-robe-selection-face
       '((t :inherit ac-selection-face
            :foreground "#CC342D"))
       "Face for robe selection."
       :group 'auto-complete)

     (add-hook 'robe-mode-hook
               (lambda ()
                 (add-to-list 'ac-source-robe
                              '(candidate-face . ac-robe-candidate-face))
                 (add-to-list 'ac-source-robe
                              '(selection-face . ac-robe-selection-face))))))


(resolve init-robe-mode)
