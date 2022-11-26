
;;;; init-dired-hacks.el


(premise init)
(premise mode-line)
(premise inst-dired-hacks)

(eval-when-compile (require 'dired))

(declare-function dired-subtree-overlays-put-priority t t)

(with-eval-after-load 'dired-filter
  (modify-minor-mode-lighter 'dired-filter-mode ""))

(with-eval-after-load 'dired-subtree
  (defun dired-subtree-overlays-put-priority (symbol newval operation where)
    "Watching `dired-subtree-overlays' to put priority as zero."
    (if (eq operation 'set)
        (mapc (lambda (overlay)
                (overlay-put overlay 'priority -51))
              newval)))
  (add-variable-watcher 'dired-subtree-overlays
                        #'dired-subtree-overlays-put-priority))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "/") dired-filter-map)

  (define-key dired-mode-map (kbd "@@") #'dired-narrow)
  (define-key dired-mode-map (kbd "@%") #'dired-narrow-regexp)
  (define-key dired-mode-map (kbd "%@") #'dired-narrow-regexp)
  (define-key dired-mode-map (kbd "@~") #'dired-narrow-fuzzy)

  (define-key dired-mode-map (kbd "; F") #'dired-subtree-insert)
  (define-key dired-mode-map (kbd "; B") #'dired-subtree-remove)
  (define-key dired-mode-map (kbd "; b") #'dired-subtree-up)
  (define-key dired-mode-map (kbd "; f") #'dired-subtree-down)
  (define-key dired-mode-map (kbd "; n") #'dired-subtree-next-sibling)
  (define-key dired-mode-map (kbd "; p") #'dired-subtree-previous-sibling)
  (define-key dired-mode-map (kbd "; M-<") #'dired-subtree-beginning)
  (define-key dired-mode-map (kbd "; M->") #'dired-subtree-end)
  (define-key dired-mode-map (kbd "; m") #'dired-subtree-mark-subtree)
  (define-key dired-mode-map (kbd "; u") #'dired-subtree-unmark-subtree)
  (define-key dired-mode-map (kbd "; o f") #'dired-subtree-only-this-file)
  (define-key dired-mode-map (kbd "; o d") #'dired-subtree-only-this-directory))


(resolve init-dired-hacks)
