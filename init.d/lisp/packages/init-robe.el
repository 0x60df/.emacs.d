
;;;; init-robe.el


(premise init)
(premise mode-line)
(premise feature)
(premise init-inf-ruby)
(premise inst-robe)

(eval-when-compile (require 'company))

(lazy-autoload 'robe-start "robe")

(declare-function ac-delete-duplicated-candidates "auto-complete")

(with-eval-after-load 'ac-robe
  (require 'auto-complete nil t)

  (advice-add 'ac-robe-candidates
              :filter-return #'ac-delete-duplicated-candidates)

  (defface ac-robe-candidate-face
    '((t :inherit ac-candidate-face
         :foreground "#CC342D"))
    "Face for robe candidate."
    :group 'user)

  (defface ac-robe-selection-face
    '((t :inherit ac-selection-face
         :foreground "#CC342D"))
    "Face for robe selection."
    :group 'user)

  (add-to-list 'ac-source-robe
               '(candidate-face . ac-robe-candidate-face))
  (add-to-list 'ac-source-robe
               '(selection-face . ac-robe-selection-face)))

(with-eval-after-load 'robe
  (with-eval-after-load 'company
    (letrec ((find-capf-cell
              (lambda (l)
                (cond ((null l) l)
                      ((or (eq 'company-capf (cadr l))
                           (and (listp (cadr l))
                                (eq 'company-capf (caadr l))))
                       l)
                      (t (funcall find-capf-cell (cdr l)))))))
      (let ((cell (funcall find-capf-cell company-backends)))
        (setcdr cell
                (cons
                 '(company-robe
                   :with
                   company-dabbrev-code company-keywords company-yasnippet)
                 (cdr cell)))))))

(push '(robe-mode . 2) mode-line-minor-mode-priority-alist)
(with-eval-after-load 'robe
  (modify-minor-mode-lighter
   'robe-mode '((" ro" (:eval (if (robe-running-p) "+" "-"))))))

(defun robe-boot ()
  "Setup robe and start."
  (interactive)
  (robe-mode)
  (save-window-excursion
    (inf-ruby)
    (let ((process (get-buffer-process (current-buffer))))
      (if (process-live-p process)
          (set-process-query-on-exit-flag process nil))))
  (robe-start))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c a") #'robe-boot))


(resolve init-robe)
