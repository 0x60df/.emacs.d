
;;;; init-org.el


(premise init)
(premise custom)
(premise bindings)

(custom-set-variables
 '(org-use-speed-commands t))

(overriding-set-key (kbd "C-c o a") #'org-agenda)
(overriding-set-key (kbd "C-c o c") #'org-capture)


(resolve init-org)
