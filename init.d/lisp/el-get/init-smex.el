
;;;; init-smex.el



;;; base

(premise init)
(premise inst-smex)

(smex-initialize)


;;; bindings

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'execute-extended-command)

(defadvice smex-prepare-ido-bindings (after
                                      smex-prepare-additional-ido-bindings)
  (define-key ido-completion-map " " 'ido-next-match)
  (define-key ido-completion-map (kbd "S-SPC") 'ido-prev-match))
(ad-activate 'smex-prepare-ido-bindings)


(resolve init-smex)
