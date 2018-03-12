
(let ((minver "25.1"))
  (when (version< emacs-version minver)
(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

                                        ; elpa.el
;; Initialize package repo's
(require 'package)
(setq package-list '(use-package diminish bind-key))

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

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

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

(defun ana--run-prettier ()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively
     (async-shell-command "git --no-pager diff --name-only origin | xargs prettier --config ./.prettierrc.yml --insert-pragma --write"))))

(use-package general :ensure t
  :diminish ""
  :init
  (general-define-key :states '(normal motion emacs insert) "<left>" nil "<right>" nil "<up>" nil "<down>" nil)
  (general-define-key :states '(normal motion emacs) "SPC" nil)
  (general-define-key
   :keymaps 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  (general-define-key
   :states '(motion emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ;; M-x
   "SPC" '(counsel-M-x :which-key "M-x")

   ;;Buffers
   "b" '(:ignore t :which-key "buffer")
   "bs" '(ivy-switch-buffer :which-key "switch")
   "bp" '(previous-buffer :which-key "previous")
   "bl" '(buffer-list :which-key "list")
   "bn" '(next-buffer :which-key "next")
   "bk" '(kill-buffer :which-key "delete")
   "bd" '(kill-this-buffer :which-key "delete")

   ;; Finder
   "f" '(:ignore t :which-key "find")
   "ff" '(counsel-find-file :which-key "file")
   "fl" '(counsel-locate :which-key "locate")
   "fd" '(dictionary-search :which-key "definition"))

  (general-define-key
   :states 'motion
   :prefix "SPC"
   ;; Testing commands
   "t" '(:ignore t :which-key "test command")
   "tp" '(ana--run-prettier :which-key "prettier")

   ;; Org-mode
   "o" '(:ignore t :which-key "org-mode")
   "oc" '(org-capture :which-key "capture")
   "oa" '(org-agenda :which-key "agenda")

   ;; Comments
   "c" '(:ignore t :which-key "comment")
   "cl" '(comment-line :which-key "line")
   "cr" '(comment-region :which-key "region")
   "cb" '(comment-box :which-key "box")

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
   "ut" '(counsel-load-theme :which-key "change theme"))

  ;; Major-mode keybindings
  ;; Rust
  (general-define-key
   ;; :keymaps 'rust-mode
   :states 'motion
   :prefix "SPC"
   "mb" 'cargo-process-build
   "mr" 'cargo-process-run
   "mt" 'cargo-process-test
   "mf" 'rust-format-buffer))

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

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; Syntax checking
(use-package flycheck
  :ensure t
  :diminish ""
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
(diminish 'eldoc-mode)

                                        ; Modes config
;; Org-mode
(use-package org
  :ensure t
  :commands (org-capture org-agenda)
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

  (setq org-pretty-entities t))

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
  (use-package evil-magit :ensure t))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode emmet-mode rainbow-mode web-mode tide rjsx-mode npm-mode json-mode evil-magit magit org-bullets counsel-projectile counsel projectile flycheck exec-path-from-shell dictionary company powerline zenburn-theme challenger-deep-theme spacemacs-theme general which-key evil-indent-plus evil-surround evil-escape no-easy-keys nlinum-relative diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
