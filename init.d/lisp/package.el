
;;;; package.el


(premise init)
(premise custom)

(custom-set-variables
 '(package-archives
   (let ((package-archive '("melpa" . "https://melpa.org/packages/")))
     (if (member package-archive package-archives)
         package-archives
       (cons package-archive package-archives)))))

(package-initialize)


(resolve package)
