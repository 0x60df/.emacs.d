
;;;; package.el


(premise init)

(require 'package)

(setq package--init-file-ensured t)
(package-initialize)


(resolve package)
