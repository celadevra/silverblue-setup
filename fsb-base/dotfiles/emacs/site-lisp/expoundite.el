;;; expoundite.el: settings for managing expoundite.net from Emacs

;; code here requires `markdown-mode' and `deft' installed.

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq deft-extensions '("mdwn" "txt"))
(setq deft-directory "~/Documents/expoundite.net/")
(setq deft-recursive t)
(defun deft-find-file ()
  (interactive)
  (org-capture nil "m")) ; require xhy-notetaking.el

(my-leader-def
  "d" '(:ignore t :which-key "deft")
  "dd" 'deft)

(general-define-key
 :states 'normal
 :keymaps 'deft-mode-map
 :prefix "SPC"
 "df" 'deft-find-file
 "dx" 'deft-delete-file
 "dg" 'deft-refresh
 "dq" 'quit-window
 "dc" 'deft-filter-clear)

(provide 'expoundite)
