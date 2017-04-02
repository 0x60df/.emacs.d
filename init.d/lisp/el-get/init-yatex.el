
;;;; init-yatex.el



;;; base

(premise init)
(premise inst-yatex)

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode))
              auto-mode-alist))

(setq YaTeX-close-paren-always 'never)
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)
             (reftex-mode 1)))


(resolve init-yatex)
