                                        ; Emacs 25
(message "=== Initializing Emacs ===")
;; Version check
(let ((minver "25.1"))
  (when (version< emacs-version minver)
(error "Emacs is too old -- this config requires v%s or higher" minver)))

;; Emacs configuration location
(defvar ana--dotfiles "~/dotfiles/configs/")

;; Change to home directory (needed when running Emacs via chocolaty)
(cd "~/")

                                        ; elpa.el
;; Initialize package repo's
(require 'package)
(setq package-enable-at-startup nil)
(defvar package-list '(use-package delight))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Install new package versions if available
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(eval-when-compile
  (require 'use-package)
  (require 'delight))

                                        ; Essential Setting
(message "=== Configuring sane defaults ===")
;; Sensible defaults
(load-file (concat ana--dotfiles "emacs.d/lisp/sensible-defaults.el"))
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

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
(setq-default tab-always-indent 'complete)
(when window-system
  (global-hl-line-mode))
(column-number-mode)
(setq-default fill-column 90)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-mode-hook '(lambda ()
                                   (setq-default fill-column 72)
                                   (turn-on-auto-fill)))
(setq frame-title-format '((:eval (projectile-project-name))))
(global-prettify-symbols-mode t)
(setq scroll-conservatively 100)

                                        ; User configuration
(setq
  user-full-name "Kevin aka venikx"
  user-mail-address "kevin.rangel@protonmail.com"
  calendar-location-name "Helsinki, Finland - Europe")

(message "=== Starting... ===")

                                        ; Evil Config
;; Disable easy keys, to properly learn emacs/evil keybindings
(use-package no-easy-keys
  :ensure t
  :config (no-easy-keys 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;;  Amazing collection of evil bindings for several packages
(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init '(calender company dired ivy)))

(use-package evil-magit
  :ensure t
  :after evil magit)

(use-package evil-org
  :disabled
  :ensure t
  :after evil)

(use-package evil-surround
  :ensure t
  :after evil
  :delight evil-surround-mode
  :config (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :after evil
  :delight evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion)))

                                        ; Use general.el and which-keys.el to structure keybindings
(use-package which-key
  :ensure t
  :init (which-key-mode t))

(defun ana--run-prettier ()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively
     (async-shell-command
      "prettier --config ./.prettierrc.yml --require-pragma "src/**/*.js" --write"))))

(use-package general :ensure t
  :config
  (general-define-key :states '(normal motion emacs) "SPC" nil)

  ;; Global overrides
  (general-define-key
   "<left>" nil "<right>" nil "<up>" nil "<down>" nil
   "M-x" 'counsel-M-x
   "C-s" 'counsel-grep-or-swiper
   "<f2> l" 'counsel-find-library
   "<f2> u" 'counsel-unicode-char)

  ;; C-x overrides
  (general-define-key
   "C-x C-f" 'counsel-find-file
   "C-x C-b" 'ivy-switch-buffer
   "C-x b" 'ibuffer-list-buffers
   "C-x k" 'ido-kill-buffer)

  ;; General
  (general-define-key
   :states '(motion emacs)
   :prefix "SPC"
   :global-prefix "C-SPC"
   ;; M-x
   "SPC" '(counsel-M-x :which-key "M-x")

   ;; Git
   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status

   ;; Projectile
   "p" '(:ignore t :which-key "project")
   "pa" '(counsel-projectile-ag :which-key "ag")
   "pg" '(counsel-projectile-ag :which-key "grep")
   "pp" '(counsel-projectile-switch-project :which-key "switch prj")
   "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
   "pf" '(counsel-projectile-find-file :which-key "find file")

   ;; Org-mode
   "o" '(:ignore t :which-key "org")
   "oc" 'org-capture
   "oa" 'org-agenda

   ;; Finder
   "f" '(:ignore t :which-key "find")
   "ff" 'counsel-find-file
   "fl" 'counsel-locate
   "fd" 'dictionary-search

   ;; Comments
   "c" '(:ignore t :which-key "comment")
   "cl" 'comment-line
   "cr" 'comment-region
   "cb" '(comment-box "box")

   ;; UI config
   "u" '(:ignore t :which-key "UI")
   "ut" '(counsel-load-theme :which-key "change theme")

   ;; Testing commands
   "t" '(:ignore t :which-key "danger zone"))

  ;; Major-mode keybindings (SPC-m then brings up context sensitive keybindings)
  (general-define-key
   :keymaps 'rjsx-mode-map
   :states 'motion
   :prefix "SPC m"
   "p" 'ana--run-prettier)

  (general-define-key
   :keymaps 'org-mode-map
   :states 'motion
   :prefix "SPC m")

  (general-define-key
   :keymaps 'rust-mode-map
   :states 'motion
   :prefix "SPC m"
   "b" 'cargo-process-build
   "r" 'cargo-process-run
   "t" 'cargo-process-test
   "f" 'rust-format-buffer))



                                        ; Utils
(use-package exec-path-from-shell
 :ensure t
 :if (memq window-system '(mac ns x))
 :init (exec-path-from-shell-initialize))

;; Emacs Completion
(use-package ivy
  :ensure t
  :defer 0.1
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d")
  (ivy-height 20)
  (projectile-completion-system 'ivy))

(use-package ivy-rich
  :ensure t
  :delight ivy-rich-mode
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :delight counsel-mode
  :after ivy
  :config
  (counsel-mode 1))

(use-package counsel-etags
  :delight counsel-etags-mode
  :ensure t
  :after counsel
  :config
  (counsel-projectile-mode))

(use-package counsel-projectile
  :delight counsel-projectile-mode
  :ensure t
  :after counsel
  :config
  (counsel-projectile-mode))

(use-package swiper
  :ensure t
  :after ivy)

;; Code completion
(use-package company
  :ensure t
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0))

;; Projectile
(use-package projectile
  :ensure t
  :defer 1
  :init (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"(projectile-project-name))))
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil))


;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :commands (flycheck-mode)
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (setq-default flycheck-disabled-checker 'javascript-eslint)
  (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;; Dictionary
(use-package dictionary :ensure t)

                                        ; Modes config
;; Org-mode
(use-package org
  :ensure t
  :commands (org-capture org-agenda)
  :config
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "APPT(a)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "CANCELLED(c@/!)")))

  (setq org-tag-alist (quote (("@errand" . ?e)
                              ("@mari" . ?m)
                              ("@reading" . ?r)
                              ("@computer" . ?c)
                              ("@famoco" . ?f)
                              ("@home" . ?h))))
  (setq org-fast-tag-selection-single-key nil)

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "light coral" :weight bold)
          ("NEXT" :foreground "red" :weight bold)
          ("DONE" :foreground "sea green")
          ("APPT" :foreground "maroon")
          ("WAITING" :foreground "dark orange" :weight bold)
          ("CANCELLED" :foreground "dim gray")
          ("HOLD" :foreground "deep sky blue" :weight bold)))

  (defvar org-capture-templates
        '(("t" "Todo" entry (file org-default-notes-file)
           "* TODO %? \nAdded: %U\n")
          ("n" "Next" entry (file org-default-notes-file)
           "* NEXT %? \nDEADLINE: %t")
          ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
           "* %?\n" :clock-in t :clock-resume t)))

  (setq org-default-notes-file "~/Documents/org/gsd/inbox.org")
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files '("~/Documents/org/gsd/gsd.org"))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("gsd.org" :maxlevel . 1)
                             ("someday.org" :maxlevel . 1)))

  (setq org-pretty-entities t))

(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-format "%s"))


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-ellipsis "⤵")
  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))

;; Magit
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq git-commit-summary-max-length 50)
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

                                        ; Code
;; Javascript
(use-package json-mode :ensure t)
(use-package npm-mode :ensure t)
(defun add-node-modules-path ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (message "added node_modules to exec-path"))
      (message "node_modules not found"))))

(use-package rjsx-mode
  :ensure t
  :config
  (setq js2-basic-offset 2)
  (setq js2-mode-toggle-warnings-and-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package tide
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (tide-mode +1)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package web-mode
  :ensure t
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (use-package rainbow-mode
    :ensure t
    :commands rainbow-mode)

  (use-package emmet-mode
    :ensure t
    :commands emmet-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Rust
(use-package rust-mode
  :ensure t
  :commands (rust-format-buffer)
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Ledger
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)

  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode "\\.dat\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

                                        ; Specific UI/UX
(use-package fill-column-indicator
  :ensure t
  :config (add-hook 'text-mode-hook #'fci-mode))

(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

;; Themes
(use-package spacemacs-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(load-theme 'challenger-deep t)

;; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-count-format "%d/%d")
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   (quote
    (evil-magit yaml-mode ledger-mode cargo racer flycheck-rust rust-mode markdown-mode emmet-mode rainbow-mode web-mode tide rjsx-mode npm-mode json-mode magit org-bullets org-pomodoro counsel-projectile counsel projectile flycheck exec-path-from-shell dictionary company powerline zenburn-theme challenger-deep-theme spacemacs-theme general which-key evil-escape evil-surround evil-collection evil no-easy-keys nlinum-relative fill-column-indicator diminish use-package)))
 '(projectile-completion-system (quote ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
