
;;;; init-tree-sitter.el


(premise init)
(premise mode-line)
(premise inst-tree-sitter)

(push '(tree-sitter-mode . 46) mode-line-minor-mode-priority-alist)

(with-eval-after-load 'tree-sitter
  (modify-minor-mode-lighter 'tree-sitter-mode " TreS"))


(resolve init-tree-sitter)
