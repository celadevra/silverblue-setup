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
(set-language-environment 'utf-8)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)

(use-package diminish :ensure t)

(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-firefox-program "flatpak-spawn"
      browse-url-firefox-arguments '("--host" "flatpak" "run"
      "org.mozilla.firefox"))

;; * Evil-mode and keybindings
(use-package evil :ensure t
  :config
  (evil-mode))
(use-package which-key :ensure t
  :config
  (which-key-mode)
  (setq which-key-enable-extended-define-key t))
(use-package general :ensure t
  :config
  (general-override-mode)
  (general-auto-unbind-keys))
(general-create-definer my-leader-def
  :states '(normal insert visual hybrid replace emacs)
  :prefix "SPC"
  :non-normal-prefix "C-c SPC")
(general-create-definer my-local-leader-def
  :prefix "SPC m")
(my-leader-def
  "f" '(:ignore t :which-key "file")
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "x"  'counsel-M-x
  "b" '(:ignore t :which-key "buffer")
  "bb" 'ivy-switch-buffer
  "bd" 'evil-delete-buffer
  "bp" 'evil-prev-buffer
  "bn" 'evil-next-buffer
  "bi" 'ibuffer)

;; * Git
(use-package magit :ensure t
  :config
  (my-leader-def
    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status
    "gb" 'magit-branch))

;; * Counsel and Ivy
(use-package counsel :ensure t
  :diminish "cl")
(use-package ivy :ensure t
  :diminish "iv"
  :config
  (ivy-mode 1)
  (counsel-mode)
  (general-def
    '(normal visual insert replace)
    'global
    "C-s" 'swiper)
  (my-leader-def
    "h"  '(:ignore t :which-key "help")
    "ha" 'counsel-apropos
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

;; * Projectile
(use-package projectile :ensure t
  :diminish
  :config
  (projectile-mode +1)
  (my-leader-def
    "P" '(:keymap projectile-command-map :which-key "projectile")))
(setq projectile-completion-system 'ivy)

;; * Note-taking system
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(use-package org :ensure t)
(require 'xhy-notetaking)

;; * Website managing
(use-package markdown-mode :ensure t)
(use-package deft :ensure t)
(require 'expoundite)

;; * Racket programming
(use-package racket-mode :ensure t)
(use-package scribble-mode :ensure t)
(require 'xhy-racket)

;; * Fonts
;; Align Test here.
;; 中英文对齐之测试
(if (display-graphic-p)
    (progn ;; set English font
      (set-face-attribute 'default nil :font "IBM Plex Mono Text-10")
      ;; Chinese font
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font)
			  charset (font-spec :family "Noto Sans SC"
					     :size 10)))
      (set-fontset-font t 'unicode "Symbola" nil 'prepend)
      (setq face-font-rescale-alist '(("Noto Sans SC" . 1.6)))))

;; * Theme
(use-package gruvbox-theme :ensure t
  :config
  (load-theme 'gruvbox-light-hard))
(use-package powerline :ensure t
  :config
  (require 'powerline)
  (powerline-center-evil-theme))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default))
 '(org-agenda-files
   '("/var/home/xhy/Documents/org/2020-07.org" "/var/home/xhy/Documents/org/2013-10.org" "/var/home/xhy/Documents/org/2016-03.org" "/var/home/xhy/Documents/org/2016-04.org" "/var/home/xhy/Documents/org/2018-06.org" "/var/home/xhy/Documents/org/2018-07.org" "/var/home/xhy/Documents/org/2018-08.org" "/var/home/xhy/Documents/org/2018-10.org" "/var/home/xhy/Documents/org/2018-11.org" "/var/home/xhy/Documents/org/2019-04.org" "/var/home/xhy/Documents/org/2019-05.org" "/var/home/xhy/Documents/org/2019-06.org" "/var/home/xhy/Documents/org/2019-07.org" "/var/home/xhy/Documents/org/2019-08.org" "/var/home/xhy/Documents/org/2019-09.org" "/var/home/xhy/Documents/org/2019-12.org" "/var/home/xhy/Documents/org/2020-01.org" "/var/home/xhy/Documents/org/2020-02.org" "/var/home/xhy/Documents/org/2020-03.org" "/var/home/xhy/Documents/org/2020-04.org" "/var/home/xhy/Documents/org/2020-05.org" "/var/home/xhy/Documents/org/2020-06.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
