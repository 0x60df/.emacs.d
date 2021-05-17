
;;;; init-flycheck-package.el


(premise init)
(premise inst-flycheck-package)

(with-eval-after-load 'flycheck
  (flycheck-package-setup))


(resolve init-flycheck-package)
