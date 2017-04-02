
;;;; init-inf-ruby.el



;;; base

(premise init)
(premise inst-inf-ruby)

(eval-when-compile (require 'ruby-mode))

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(defun inf-ruby-other-window ()
  (interactive)
  (let ((window (selected-window)))
    (inf-ruby)
    (select-window window)))


;;; bindings

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "\C-ccr" 'inf-ruby-other-window)))


(resolve init-inf-ruby)
