
;;;; package.el


(premise init)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)


(resolve package)
