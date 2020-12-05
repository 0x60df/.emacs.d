
;;;; init-org.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)

(custom-set-variables
 '(org-use-speed-commands t))

(push '(org-indent-mode . 1) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'org-indent
  (setcar (cdr (assq 'org-indent-mode minor-mode-alist)) " in"))

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)


(resolve init-org)
