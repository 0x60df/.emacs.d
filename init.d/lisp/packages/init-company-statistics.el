
;;;; init-company-statistics.el


(premise init)
(premise init-company)
(premise inst-company-statistics)

(with-eval-after-load 'company-statistics
  (add-hook 'company-statistics-mode-hook
            (lambda (&rest args)
              (setq company-transformers
                    (delq 'company-replace-yasnippet-candidate-on-first
                          company-transformers))
              (if company-statistics-mode
                  (add-to-list
                   'company-transformers
                   'company-replace-yasnippet-candidate-on-first 'append)))))

(with-eval-after-load 'company
  (company-statistics-mode))


(resolve init-company-statistics)
