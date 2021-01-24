
;;;; init-web-mode.el


(premise init)
(premise custom)
(premise inst-web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(custom-set-variables
 '(web-mode-enable-comment-annotation t)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-block-face t)
 '(web-mode-enable-part-face t)
 '(web-mode-enable-inlays t)
 '(web-mode-enable-element-tag-fontification t)
 '(web-mode-enable-element-content-fontification t)
 '(web-mode-enable-html-entities-fontification t)
 '(web-mode-jsx-depth-faces
   '(web-mode-jsx-depth-1-face
     web-mode-jsx-depth-2-face
     web-mode-jsx-depth-3-face
     web-mode-jsx-depth-4-face
     web-mode-jsx-depth-5-face)))


(resolve init-web-mode)
