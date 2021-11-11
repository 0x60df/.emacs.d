
;;;; init-git-gutter-fringe.el


(premise init)
(premise advice)
(premise frame)
(premise inst-git-gutter-fringe)

(add-hook-for-once 'after-make-terminal-functions
                   (lambda (_)
                     (if (and (boundp 'git-gutter-mode) git-gutter-mode)
                         (require 'git-gutter-fringe)
                       (advice-add-for-once
                        'git-gutter-mode
                        :before
                        (lambda (&rest args) (require 'git-gutter-fringe))))))


(resolve init-git-gutter-fringe)
