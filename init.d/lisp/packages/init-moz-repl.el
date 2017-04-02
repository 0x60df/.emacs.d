
;;;; init-moz-repl.el


(premise init)
(premise inst-moz-repl)

(eval-when-compile (require 'moz))

(autoload 'inferior-moz-process "moz"
  "Return inferior MozRepl process.  Start it if necessary.
See also `inferior-moz-start-process'." t)

(defun moz-send-message (moz-command)
  (comint-send-string
   (inferior-moz-process)
   (concat moz-repl-name ".pushenv('printPrompt', 'inputMode');"
           moz-repl-name ".setenv('inputMode', 'line');"
           moz-repl-name ".setenv('printPrompt', false); undefined;\n"))
  (comint-send-string
   (inferior-moz-process)
   (concat moz-command
           moz-repl-name ".popenv('inputMode', 'printPrompt'); undefined;\n")))

(defcustom moz-functions-directory "~/.emacs.d/moz-functions" "" :group 'user)

(defun moz-turn-out-call-form (name &rest arguments)
  (let* ((carrier (intern (concat "moz-function-" name)))
         (definition (progn
                       (if (not (boundp carrier)) (moz-load-function name))
                       (symbol-value carrier))))
    (format "(%s(%s))"
            definition
            (mapconcat 'identity arguments ", "))))

(defun moz-load-function (name)
  (let ((carrier (intern (concat "moz-function-" name))))
    (set carrier
         (with-temp-buffer
           (insert-file-contents
            (format "%s/%s.js" moz-functions-directory name))
           (replace-regexp-in-string
            "\n" ""
            (replace-regexp-in-string
             "^\\s-+\\|\\s-+$" ""
             (buffer-substring-no-properties
              (point-min) (point-max))))))))

(defun moz-scrolldown-1 ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollLineDown');"))

(defun moz-scrolldown ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollPageDown');"))

(defun moz-scrollup-1 ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollLineUp');"))

(defun moz-scrollup ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollPageUp');"))

(defun moz-top ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollTop');"))

(defun moz-bottom ()
  (interactive)
   (moz-send-message "goDoCommand('cmd_scrollBottom');"))

(defun moz-reload ()
  (interactive)
  (moz-send-message "BrowserReload();"))

(defun moz-next-tab ()
  (interactive)
  (moz-send-message "getBrowser().mTabContainer.advanceSelectedTab(1, true);"))

(defun moz-previous-tab ()
  (interactive)
  (moz-send-message "getBrowser().mTabContainer.advanceSelectedTab(-1, true);"))

(defun moz-new-tab ()
  (interactive)
  (moz-send-message "gBrowser.selectedTab = gBrowser.addTab();"))

(defun moz-close-tab ()
  (interactive)
  (moz-send-message "BrowserCloseTabOrWindow();"))

(defun moz-undo-close-tab ()
  (interactive)
  (moz-send-message "undoCloseTab();"))

(defcustom moz-search-engine-format "https://www.google.com/search?q=%s" ""
  :group 'user)

(defun moz-search (exp)
  (interactive "sSearch: ")
  (moz-send-message (format "content.location='%s';"
                            (format
                             moz-search-engine-format (url-encode-url exp)))))

(defcustom moz-highlight-background-color "#117722" "" :group 'user)
(defcustom moz-highlight-foreground-color "#ffffff" "" :group 'user)

(defun moz-focus-link-on-top ()
  (interactive)
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form "getLinkDisplayedAtTop")
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-link-on-bottom ()
  (interactive)
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form "getLinkDisplayedAtBottom")
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-link-on-middle ()
  (interactive)
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form "getLinkDisplayedAtMiddle")
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-next-link (&optional arg)
  (interactive "p")
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form
                             "getLinkForward" (number-to-string arg))
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-previous-link (&optional arg)
  (interactive "p")
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form
                             "getLinkForward" (number-to-string (* -1 arg)))
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-link-below-active ()
  (interactive)
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form "getLinkBelowActive")
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-link-above-active ()
  (interactive)
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form "getLinkAboveActive")
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-focus-matched-link (pattern)
  (interactive "spattern: ")
  (moz-send-message
   (concat
    (moz-turn-out-call-form "focusAndHighlight"
                            (moz-turn-out-call-form
                             "getMatchedLink"
                             (format "'%s'" pattern))
                            (format "'%s'" moz-highlight-foreground-color)
                            (format "'%s'" moz-highlight-background-color))
    ";\n")))

(defun moz-follow-focused-link ()
  (interactive)
  (moz-send-message
   (concat "content.location="
           "content.document.activeElement.getAttribute('href');")))

(defun moz-open-focused-link-in-new-tab ()
  (interactive)
  (moz-send-message
   (concat "content.open("
           "content.document.activeElement.getAttribute('href'));")))

(defun moz-history-back ()
  (interactive)
  (moz-send-message "content.history.back();"))


(resolve init-moz-repl)
