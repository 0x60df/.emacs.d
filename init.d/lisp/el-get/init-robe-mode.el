
;;;; init-robe-mode.el



;;; base

(eval-when-compile
  (require 'ruby-mode)
  (require 'robe))


;;; functions

(defun robe-boot ()
  (interactive)
  (ac-robe-setup)
  (robe-mode)
  (run-ruby)
  (delete-window)
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
