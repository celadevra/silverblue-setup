;; * Bootstraping Package system
(server-start)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; * Evil-mode and keybindings
(use-package evil :ensure t
  :config
  (evil-mode))
(use-package general :ensure t
  :config
  (general-create-definer my-leader-def
    :prefix "SPC")
  (general-create-definer my-local-leader-def
    :prefix "SPC m")
  (my-leader-def
   :keymaps 'normal
   "ff" 'find-file
   "fs" 'save-buffer
   "x"  'counsel-M-x))

;; * Git
(use-package magit :ensure t
  :config
  (my-leader-def
   :keymaps 'normal
   "gg" 'magit-status
   "gb" 'magit-branch))

;; * Theme
(use-package gruvbox-theme :ensure t
  :config
  (load-theme 'gruvbox-dark-hard))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
