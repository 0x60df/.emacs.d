
;;;; init-org.el


(premise init)
(premise custom)
(premise mode-line)
(premise bindings)

(eval-when-compile (require 'org))
(declare-function org-time-stamp "org")
(declare-function org-time-stamp-inactive "org")

(custom-set-variables
 '(org-use-speed-commands t)
 '(org-highlight-latex-and-related '(latex script entities)))

(push '(org-indent-mode . 1) mode-line-minor-mode-priority-alist)

(defun org-hide-trailing-whitespace-for-export-dispatcher (buffer &rest args)
  "Advising `org-switch-to-buffer-other-window' to hide whitespace."
  (if (eq buffer (get-buffer "*Org Export Dispatcher*"))
      (with-current-buffer "*Org Export Dispatcher*"
        (setq show-trailing-whitespace nil))))

(advice-add 'org-switch-to-buffer-other-window
            :before #'org-hide-trailing-whitespace-for-export-dispatcher)

(with-eval-after-load 'org-indent
  (setcar (cdr (assq 'org-indent-mode minor-mode-alist)) " ind"))

(defun enhance-org-capture-header-line-format (&rest args)
  "Advising `org-capture-mode' to make `header-line-format' enhanced.
Specifically, add front space and setup auto truncate. "
  (setq header-line-format (mode-line-format-auto-truncate
                            (list mode-line-front-space header-line-format))))

(advice-add 'org-capture-mode :after #'enhance-org-capture-header-line-format)

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-.") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c C-!") #'org-time-stamp-inactive))


(resolve init-org)
