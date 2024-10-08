
;;;; init-company-statistics.el


(premise init)
(premise init-company)
(premise inst-company-statistics)

(eval-when-compile (require 'company))

(custom-set-variables
 '(company-transformers '(company-force-prefix-when-capf
                          company-sort-by-length
                          (lambda (candidates)
                            (let ((first (car candidates))
                                  (by-occurrence (company-sort-by-occurrence
                                           candidates)))
                              (if (equal first (car by-occurrence))
                                  (company-sort-by-statistics by-occurrence)
                                (cons (car by-occurrence)
                                      (company-sort-by-statistics
                                       (cdr by-occurrence))))))
                          company-replace-yasnippet-candidate-on-first)))

(with-eval-after-load 'company-statistics
  (add-hook 'company-statistics-mode-hook
            (lambda (&rest args)
              (when company-statistics-mode
                (setq company-transformers
                      (remq 'company-replace-yasnippet-candidate-on-first
                            company-transformers))
                (add-to-list
                 'company-transformers
                 'company-replace-yasnippet-candidate-on-first 'append)))))

(with-eval-after-load 'company
  (company-statistics-mode))


(resolve init-company-statistics)
