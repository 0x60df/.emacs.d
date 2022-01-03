
;;;; init-helm-bibtex.el


(premise init)
(premise inst-helm-bibtex)

(with-eval-after-load 'helm-bibtex
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation"
                             'helm-bibtex-insert-citation
                             helm-source-bibtex 0))


(resolve init-helm-bibtex)
