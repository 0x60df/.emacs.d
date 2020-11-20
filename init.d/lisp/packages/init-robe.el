
;;;; init-robe.el


(premise init)
(premise mode-line)
(premise feature)
(premise init-auto-complete)
(premise init-inf-ruby)
(premise inst-robe)

(lazy-autoload 'robe-start "robe")

(advice-add 'ac-robe-candidates
            :filter-return #'ac-delete-duplicated-candidates)

(with-eval-after-load 'ac-robe
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

(push '(robe-mode . 2) mode-line-minor-mode-priority-alist)
(with-eval-after-load 'robe
  (setcdr (assq 'robe-mode minor-mode-alist)
          '((" ro" (:eval (if (robe-running-p) "+" "-"))))))

(defun robe-boot ()
  "Setup robe and start."
  (interactive)
  (robe-mode)
  (ac-robe-setup)
  (save-window-excursion (inf-ruby))
  (robe-start))

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c c a") #'robe-boot))


(resolve init-robe)
