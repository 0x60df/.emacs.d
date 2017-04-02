
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

(eval-after-load 'yatex
  '(custom-set-variables '(YaTeX-close-paren-always 'never)
                         '(YaTeX-inhibit-prefix-letter t)
                         '(YaTeX-kanji-code nil)
                         '(YaTeX-use-LaTeX2e t)
                         '(YaTeX-use-AMS-LaTeX t)))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)
             (reftex-mode 1)))


(resolve init-yatex)
