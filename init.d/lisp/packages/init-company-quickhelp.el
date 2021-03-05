
;;;; init-company-quickhelp.el


(premise init)
(premise custom)
(premise inst-company-quickhelp)

(eval-when-compile (require 'company))

(declare-function company-quickhelp--hide "company-quickhelp")
(declare-function company-quickhelp-manual-begin "company-quickhelp")

(declare-function company-quickhelp-auto-hide load-file-name t t)

(custom-set-variables
 '(pos-tip-border-width 0)
 '(pos-tip-internal-border-width 0)
 '(company-quickhelp-delay nil))

(with-eval-after-load 'company
  (defun company-quickhelp-auto-hide
      (company-quickhelp-frontend command &rest args)
    "Advising `company-quickhelp-frontend' to auto hide feature."
    (if (eq command 'pre-command)
        (company-quickhelp--hide)
      (apply company-quickhelp-frontend command args)))
  (advice-add 'company-quickhelp-frontend :around #'company-quickhelp-auto-hide)

  (company-quickhelp-mode)
  (define-key company-active-map (kbd "C-<iso-lefttab>")
    #'company-quickhelp-manual-begin))


(resolve init-company-quickhelp)
