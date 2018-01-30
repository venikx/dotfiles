                                        ; Version check
(let ((minver "25.1"))
  (when (version< emacs-version minver)
(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

                                        ; elpa.el
;; Initialize package repo's
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

;; Install new package versions if available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

                                        ; Essential Setting
;; Sensible defaults
(load-file "~/.emacs.d/lisp/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Resetting Emacs UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)

;; Global UI/UX settings
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(when window-system
  (global-hl-line-mode))
(column-number-mode)
(setq-default fill-column 80)
(setq frame-title-format '((:eval (projectile-project-name))))
(global-prettify-symbols-mode t)
(setq scroll-conservatively 100)

(use-package nlinum-relative
  :ensure t
  :init (nlinum-relative-setup-evil)
  :config
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

                                        ; User configuration
(setq user-full-name "Kevin aka Ana Robynn"
      user-mail-address "kevin.rangel@protonmail.com"
      calendar-latitude 50.8
      calendar-longitude 4.4
      calendar-location-name "Brussels, Belgium - Europe")

                                        ; Evil Config
;; Disable easy keys, to properly learn emacs/evil keybindings
(use-package no-easy-keys
  :ensure t
  :config (no-easy-keys 1))

;; Set ESC to escape from most situations
(use-package evil-escape
  :ensure t
  :init (global-set-key [escape] 'evil-escape))

;; Load evil, evil-surround
(use-package evil
  :ensure t
  :diminish ""
  :init (evil-mode 1)
  :commands (evil-mode)
  :config
  (use-package evil-surround
    :ensure t
    :diminish ""
    :init (global-evil-surround-mode 1))

  (use-package evil-indent-plus
    :ensure t
    :init (evil-indent-plus-default-bindings)))

                                        ; Use general.el and which-keys.el to structure keybindings
(use-package which-key
  :ensure t
  :diminish ""
  :init (which-key-mode t))

(use-package general :ensure t
  :diminish ""
  :config
  (general-define-key "<left>" nil "<right>" nil "<up>" nil "<down>" nil)
  (general-define-key :keymaps '(normal motion) "SPC" nil)

  (general-define-key
   :states 'insert
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC")

  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   ;;Buffers
   "b" '(:ignore t :which-key "buffer")
   "bs" '(ivy-switch-buffer :which-key "switch")
   "bk" '(kill-buffer :which-key "delete")
   "bd" '(kill-this-buffer :which-key "delete")

   ;; Finder
   "f" '(:ignore t :which-key "find")
   "ff" '(counsel-find-file :which-key "file")
   "fl" '(counsel-locate :which-key "locate")
   "fd" '(dictionary-search :which-key "definition")

   ;; Git
   "g" '(:ignore t :which-key "git")
   "gs" '(magit-status :which-key "status")

   ;; Projectile
   "p" '(:ignore t :which-key "project")
   "pa" '(counsel-projectile-ag :which-key "ag")
   "pg" '(counsel-projectile-ag :which-key "grep")
   "pp" '(counsel-projectile-switch-project :which-key "switch prj")
   "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
   "pf" '(counsel-projectile-find-file :which-key "find file")

   ;; UI config
   "u" '(:ignore t :which-key "UI")
   "ut" '(counsel-load-theme :which-key "change theme")))

                                        ; UI/UX
;; Themes
(use-package spacemacs-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(load-theme 'zenburn t)

;; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

;; Auto-complete
(use-package company
  :ensure t
  :diminish ""
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.4))

                                        ; Utils
;; Dictionary
(use-package dictionary :ensure t)

;; Syntax checking
(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Load the path into Emacs shell
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init (exec-path-from-shell-initialize))

                                        ; Emacs improvements
;; Projectile
(use-package projectile
  :ensure t
  :defer 1
  :init (projectile-mode)
  :diminish ""
  :config
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"(projectile-project-name))))
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil))

(use-package counsel
  :ensure t
  :diminish ""
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq projectile-completion-system 'ivy)

  (use-package counsel-projectile
    :ensure t
    :defer 1
    :init (counsel-projectile-mode))

  (use-package swiper
    :ensure t
    :commands swiper
    :bind ("C-s" . counsel-grep-or-swiper)
    :init (require 'counsel)))

;; Diminish certain modes
(diminish 'ivy-mode)
(diminish 'auto-revert-mode)
(diminish 'undo-tree-mode)

                                        ; Modes config
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
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
)

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck counsel-projectile projectile zenburn-theme which-key use-package spacemacs-theme rainbow-mode powerline org-jira org-bullets no-easy-keys nlinum-relative magit general exec-path-from-shell evil-leader evil-escape emmet-mode dictionary counsel company challenger-deep-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
