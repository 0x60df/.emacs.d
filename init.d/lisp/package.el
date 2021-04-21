
;;;; package.el


(premise init)
(premise advice)

(eval-when-compile (require 'package))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(advice-add-for-once 'package-install
                     :before (lambda (&rest args) (package-refresh-contents)))

(defmacro package-inst (package)
  "Install the PACKAGE unless it has been installed."
  `(unless (package-installed-p ',package)
     (package-install ',package)))


(resolve package)
