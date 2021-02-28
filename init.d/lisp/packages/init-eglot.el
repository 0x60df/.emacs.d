
;;;; init-eglot.el


(premise init)
(premise feature)
(premise bindings)
(premise inst-eglot)

(eval-when-compile (require 'eglot))

(lazy-autoload 'eglot-current-server "eglot")
(lazy-autoload 'eglot-shutdown "eglot")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'company))

(defun eglot-toggle ()
    "Toggle eglot"
    (interactive)
    (if (eglot-current-server)
        (call-interactively #'eglot-shutdown)
      (call-interactively #'eglot)))

(overriding-set-key (kbd "C-c c l") #'eglot-toggle)


(resolve init-eglot)
