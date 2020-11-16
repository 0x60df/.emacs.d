
;;;; init-inf-ruby.el


(premise init)
(premise inst-inf-ruby)

(eval-when-compile (require 'ruby-mode))

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(defun inf-ruby-other-window ()
  (interactive)
  (let ((window (selected-window)))
    (inf-ruby)
    (select-window window)))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c r") #'inf-ruby-other-window))

;;; helm support

(with-eval-after-load 'inf-ruby
  (with-eval-after-load 'helm-mode
    (add-to-list 'helm-completing-read-handlers-alist
                 '(ruby-load-file . ido-read-file-name))))


(resolve init-inf-ruby)
