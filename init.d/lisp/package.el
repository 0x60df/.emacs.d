
;;;; package.el


(premise init)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(resolve package)
