
;;;; init-org.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)

(eval-when-compile (require 'org))
(declare-function org-time-stamp "org")
(declare-function org-time-stamp-inactive "org")

(custom-set-variables
 '(org-use-speed-commands t))

(push '(org-indent-mode . 1) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'org-indent
  (setcar (cdr (assq 'org-indent-mode minor-mode-alist)) " in"))

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-.") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c C-!") #'org-time-stamp-inactive))


(resolve init-org)
