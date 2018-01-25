;; Check Emacs version
(let ((minver "25.1"))
  (when (version< emacs-version minver)
(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; General
;; Configure package repositories
(require 'package)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Install new version of use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Load sensible defaults
(load-file "~/.emacs.d/lisp/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Essential settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(when window-system
  (global-hl-line-mode))
(column-number-mode)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)

(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

;; disable easy-keys
(use-package no-easy-keys
    :ensure t
    :config
    (no-easy-keys 1))

;; Setting user config
(setq user-full-name "Kevin aka Ana Robynn"
      user-mail-address "kevin.rangel@protonmail.com"
      calendar-latitude 50.8
      calendar-longitude 4.4
      calendar-location-name "Brussels, Belgium - Europe")

;; EVIL
;; General evil config
(use-package evil-escape
    :ensure t
    :config
    (global-set-key [escape] 'evil-escape))

;; Load package
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "d" 'kill-this-buffer)))

;; UI
;; Project name as title
(setq frame-title-format "AnaRobynn")

;; Theme
(use-package zenburn-theme
  :load-path "themes"
  :config
  (load-theme 'zenburn t))

;; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))


;; UX
;; Line Formatting


;; Auto-complete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Coding
;; Defaults

;; Org-mode
(use-package org
  :ensure t
  :commands (org capture)
  :bind (("C-c c" . org-capture))
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)"
                    "CANCELLED(c@/!)")
          (sequence "MEETING(m)")
          (sequence "IDEA(i)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "light coral" :weight bold)
          ("NEXT" :foreground "firebrick" :weight bold)
          ("DONE" :foreground "sea green")
          ("WAITING" :foreground "dark orange" :weight bold)
          ("CANCELLED" :foreground "dim gray")
          ("MEETING" :foreground "maroon")
          ("IDEA" :foreground "deep sky blue" :weight bold)))

  (setq org-capture-templates
        '(("t" "Todo" entry (file org-default-notes-file)
           "* TODO %? \n%U\n")
          ("m" "Meeting" entry (file org-default-notes-file)
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("n" "Next" entry (file org-default-notes-file)
           "* NEXT %? \nDEADLINE: %t")
          ("i" "Idea" entry (file org-default-notes-file)
           "* IDEA %? \n%U")))

  (setq org-default-notes-file "~/Dropbox/org/refile.org")
  (setq org-directory "~/Dropbox/org/")
  (setq org-agenda-files '("~/Dropbox/org/"))

  (setq org-pretty-entities t)

  (evil-leader/set-key-for-mode 'org-mode
    "a"  'org-archive-subtree
    "A"  'org-agenda
    "d"  'org-deadline
    "p"  'org-set-property
    "s"  'org-schedule)

  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-ellipsis "⤵")
  (setq org-bullets-bullet-list '("•")))

;; Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader evil no-easy-keys use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
