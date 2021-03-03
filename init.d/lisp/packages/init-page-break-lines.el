
;;;; init-page-break-lines.el


(premise init)
(premise custom)
(premise init-company)
(premise inst-page-break-lines)

(eval-when-compile
  (require 'page-break-lines)
  (require 'company))

(declare-function page-break-lines--update-display-tables "page-break-lines")

(custom-set-variables
 '(page-break-lines-lighter ""))

(with-eval-after-load 'company
  (advice-add 'company-pseudo-tooltip-unless-initial-inline-frontend
              :before
              (lambda (command &rest args)
                (when (and (arrayp buffer-display-table)
                           (eq command 'post-command)
                           (and company-status
                                (not (eq company-status 'expanded))))
                  (aset buffer-display-table ?\^L
                        (vconcat (make-list company-minimum-prefix-length
                                            (make-glyph-code
                                             page-break-lines-char
                                             'page-break-lines))))
                  (add-hook-for-once
                   'company-after-completion-hook
                   (lambda (&rest args)
                     (page-break-lines--update-display-tables)))))))

(add-hook 'emacs-startup-hook #'global-page-break-lines-mode)


(resolve init-page-break-lines)
