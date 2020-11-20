
;;;; init-eldoc.el


(premise init)
(premise custom)
(premise mode-line)

(custom-set-variables
 '(eldoc-idle-delay 0.4)
 '(eldoc-minor-mode-string " ElD"))

(push '(eldoc-mode . 30) mode-line-minor-mode-priority-alist)

(defun eldoc-modify-mode-line-message (eldoc-minibuffer-message &rest args)
  "Advising `eldoc-minibuffer-message' to modify `mode-line-format'."
  (apply eldoc-minibuffer-message args)
  (when (minibufferp)
    (with-current-buffer
        (window-buffer
	 (or (window-in-direction 'above (minibuffer-window))
	     (minibuffer-selected-window)
	     (get-largest-window)))
      (unless (assq 'eldoc-mode-line-string mode-line-format-raw)
        (let* ((cell (assq 'eldoc-mode-line-string mode-line-format))
               (format (cadr cell)))
          (when (and format (not (equal format "")))
            (let ((last (last format)))
              (if (equal (car last) " ")
                  (setcar last '(:propertize " " face mode-line-separator))))
            (letrec ((insert-eldoc-after-front-space
                      (lambda (l)
                        (cond ((null l)
                               (setcdr mode-line-format-raw
                                       (cons (list 'eldoc-mode-line-string
                                                   format)
                                             (cdr mode-line-format-raw))))
                              ((eq (car l) 'mode-line-front-space)
                               (setcdr l
                                       (cons (list 'eldoc-mode-line-string
                                                   (if (equal (car format) " ")
                                                       (cons "" (cdr format))
                                                     format))
                                             (cdr l))))
                              (t (funcall insert-eldoc-after-front-space
                                          (cdr l)))))))
              (funcall insert-eldoc-after-front-space mode-line-format-raw))
            (setcdr cell '(""))
            (force-mode-line-update)))))))

(with-eval-after-load 'eldoc
  (add-to-list 'mode-line-boundary-faces 'font-lock-keyword-face)
  (add-to-list 'mode-line-boundary-faces 'font-lock-function-name-face)
  (advice-add 'eldoc-minibuffer-message
              :around #'eldoc-modify-mode-line-message))


(resolve init-eldoc)
