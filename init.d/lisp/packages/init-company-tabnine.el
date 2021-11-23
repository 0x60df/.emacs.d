
;;;; init-company-tabnine.el


(premise init)
(premise init-company)
(premise inst-company-tabnine)

(declare-function company-append-backends nil)

(with-eval-after-load 'company
  (company-append-backends 'company-capf :with 'company-tabnine)
  (company-append-backends 'company-clang :with 'company-tabnine)
  (company-append-backends 'company-dabbrev-code :with 'company-tabnine)
  (company-append-backends 'company-dabbrev :with 'company-tabnine)
  (company-append-backends 'company-tabnine))


(resolve init-company-tabnine)
