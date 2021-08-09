
;;;; init-ispell.el


(premise init)

(eval-when-compile (require 'ispell))

(with-eval-after-load 'ispell
  (advice-add 'ispell-word :before
              (lambda (&rest args)
                (with-current-buffer (get-buffer-create ispell-choices-buffer)
                  (setq show-trailing-whitespace nil)))))


(resolve init-ispell)
