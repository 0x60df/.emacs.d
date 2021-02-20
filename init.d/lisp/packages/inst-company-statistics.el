
;;;; inst-company-statistics.el


(premise init)
(premise init-el-get)

(el-get-bundle company-mode/company-statistics
  :description "Sort candidates using completion history."
  :depends (company-mode))


(resolve inst-company-statistics)
