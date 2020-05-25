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

;; * Defaults
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)

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
   "ff" 'counsel-find-file
   "fs" 'save-buffer
   "x"  'counsel-M-x
   "bb" 'ivy-switch-buffer
   "bi" 'ibuffer))

;; * Git
(use-package magit :ensure t
  :config
  (my-leader-def
   :keymaps 'normal
   "gg" 'magit-status
   "gb" 'magit-branch))

;; * Counsel and Ivy
(use-package counsel :ensure t)
(use-package ivy :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode)
  (general-def
    '(normal visual insert replace)
    'global
    "C-s" 'swiper)
  (my-leader-def
    :keymaps 'normal
    "hf" 'counsel-describe-function
    "hv" 'counsel-describe-variable
    "hi" 'counsel-info-lookup-symbol))

;; * Parentheses
(use-package smartparens :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode))
(use-package evil-smartparens :ensure t
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
(use-package rainbow-delimiters :ensure t)

;; * Company
(use-package company :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (general-def
    '(insert replace)
    'global
    "C-x C-n" 'company-complete))

;; * Theme
(use-package gruvbox-theme :ensure t
  :config
  (load-theme 'gruvbox-dark-hard))
(use-package powerline :ensure t
  :config
  (require 'powerline)
  (powerline-default-theme))

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
