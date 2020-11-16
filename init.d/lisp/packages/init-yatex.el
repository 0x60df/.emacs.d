
;;;; init-yatex.el


(premise init)
(premise feature)
(premise inst-yatex)

(eval-and-compile
  (defvar YaTeX-inhibit-prefix-letter t ; This must be set before loading.
    "Use C-c C- bindings"))             ; Inhibit usesage of C-c letter keys.

(eval-when-compile (provide 'hilit19))  ; Provide fake feature to accomplish
(lazy-autoload 'yatex-mode "yatex")     ; loading `yatex' on comile.

(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode))
              auto-mode-alist))

(with-eval-after-load 'yatex
  (setq YaTeX-close-paren-always 'never
        YaTeX-kanji-code nil
        YaTeX-use-LaTeX2e t
        YaTeX-use-AMS-LaTeX t)

  (add-hook 'yatex-mode-hook (lambda ()
                               (auto-fill-mode 0)
                               (reftex-mode))))


(resolve init-yatex)
