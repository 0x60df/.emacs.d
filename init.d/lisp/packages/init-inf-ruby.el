
;;;; init-inf-ruby.el



;;; base

(premise init)
(premise init-ido)
(premise init-helm)
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


;;; helm

(with-eval-after-load 'helm-mode
  (custom-set-variables
   '(helm-completing-read-handlers-alist
     (append
      (seq-filter (lambda (handler)
                    (not (member handler helm-completing-read-handlers-alist)))
                  '((ruby-load-file . ido-read-file-name)))
      helm-completing-read-handlers-alist))))


(resolve init-inf-ruby)
