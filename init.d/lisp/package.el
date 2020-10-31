
;;;; package.el


(premise init)
(premise custom)

(with-eval-after-load 'package
  (custom-set-variables
   '(package-archives
     (append
      package-archives
      (seq-filter (lambda (repository)
                    (not (member repository package-archives)))
                  '(("melpa" . "https://melpa.org/packages/")))))))

(package-initialize)


(resolve package)
