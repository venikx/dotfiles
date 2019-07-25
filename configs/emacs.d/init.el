;;; venikx --- init.el
;;; Author: venikx
;;; Commentary:
;;; An opinionated Emacs configuration containing all the essentials I use, while programming
;;;
;;; The initialization file is only bootstrapping the "actual" config file, which is
;;; documented in org files and passed through org-babel for Emacs to read from.
;;;
;;; Code:

;; Increase the number of bytes, before getting garbage collected
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 20000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold 40000000)))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold 20000000)))


;;; TODO(kevin): Continue
                                        ; elpa.el
;; Initialize package repo's
(require 'package)
(setq package-enable-at-startup nil
      load-prefer-newer t)
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

(setq-default use-package-verbose t
              use-package-always-ensure t)

(eval-when-compile
  (require 'use-package)
  (require 'delight))

                                        ; User configuration
(setq user-full-name "Kevin aka venikx"
      user-mail-address "kevin.rangel@protonmail.com")

                                        ; Essential Setting
(message "=== Configuring sane defaults ===")
;; Sensible defaults
(load-file (concat user-emacs-directory "lisp/sensible-defaults.el"))
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
(blink-cursor-mode)

;; Global UI/UX settings
(display-time-mode)
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
(setq frame-title-format '((:eval (projectile-project-name))))
(global-prettify-symbols-mode t)
(setq scroll-conservatively 100)

;; UTF-8 support
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(message "=== Starting... ===")

                                        ; Evil Config
;; Disable easy keys, to properly learn emacs/evil keybindings
(use-package no-easy-keys :config (no-easy-keys 1))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package origami
  :after evil
  :commands origami-mode
  :config
  (add-hook 'prog-mode-hook 'origami-mode))

;;  Amazing collection of evil bindings for several packages
(use-package evil-collection
  :after evil
  :config (evil-collection-init '(calender company ivy)))

(use-package evil-org :disabled :after evil)

(use-package evil-surround
  :after evil
  :delight evil-surround-mode
  :config (global-evil-surround-mode 1))

(use-package evil-escape
  :after evil
  :delight evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion)))

                                        ; Use general.el and which-keys.el to structure keybindings
(use-package which-key
  :defer 1
  :init (which-key-mode t))

(use-package general
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
   "gt" 'git-timemachine

   ;; Projectile
   "p" '(:ignore t :which-key "project")
   "pr" '(counsel-projectile-rg :which-key "ripgrep")
   "pb" '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
   "pf" '(counsel-projectile-find-file :which-key "find file")

   ;; Org-mode
   "o" '(:ignore t :which-key "org")
   "oc" 'org-capture
   "oa" 'org-agenda

   ;; Finder
   "f" '(:ignore t :which-key "find")
   "ff" 'counsel-find-file
   "fr" 'ranger
   "fd" 'dictionary-search

   ;; Comments
   "c" '(:ignore t :which-key "comment")
   "cl" 'comment-line
   "cr" 'comment-region
   "cb" '(comment-box "box")

   ;; UI config
   "u" '(:ignore t :which-key "UI")
   "ut" '(counsel-load-theme :which-key "change theme")
   "uf" '(focus-mode :which-key "focus")

   ;; Testing commands
   "t" '(:ignore t :which-key "danger zone")))

                                        ; Utils
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; Emacs Completion
(use-package ivy
  :delight ivy-mode
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d")
  (ivy-height 20)
  (projectile-completion-system 'ivy))

(use-package ivy-rich
  :defer 0.1
  :delight ivy-rich-mode
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package swiper :after ivy)

(use-package counsel
  :delight counsel-mode
  :after ivy
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :delight
  (counsel-projectile-mode)
  (projectile-mode '(:eval (concat "P:" (projectile-project-name))))
  :after counsel
  :custom
  (projectile-switch-project-ation 'projectile-dired)
  :config
  (counsel-projectile-mode))

(use-package ranger
  :config
  (ranger-override-dired-mode t))

;; Code completion
(use-package company
  :defer 1
  :delight company-mode
  :init (global-company-mode 1)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3))

(use-package yasnippet
  :defer 10
  :config (add-hook 'prog-mode-hook #'yas-minor-mode))
(use-package yasnippet-snippets :after yasnippet)

;; Syntax checking
(use-package flycheck
  ;; TODO(kevin): don't set flycheck globally
  :init (global-flycheck-mode)
  :commands (flycheck-mode)
  :preface
  (defun venikx/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-eslint (executable-find "eslint"))
           (local-eslint (expand-file-name "node_modules/.bin/eslint"
                                           root))
           (eslint (if (file-executable-p local-eslint)
                       local-eslint
                     global-eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (setq-default flycheck-disabled-checker 'javascript-eslint)
  (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")
  (add-hook 'rjsx-mode-hook #'venikx/use-eslint-from-node-modules)
  (add-hook 'js2-mode-hook #'venikx/use-eslint-from-node-modules)
  (add-hook 'web-mode-hook #'venikx/use-eslint-from-node-modules))

;; Dictionary
(use-package dictionary :defer t)

                                        ; Modes config
;; Org-mode
(use-package org
  :ensure org-plus-contrib
  :commands (org-capture org-agenda)
  :config
  (add-hook 'org-mode-hook
            '(lambda () (setq fill-column 100) (turn-on-auto-fill)))
  :custom
  (org-src-fontify-natively t)
  (org-hide-emphasis-markers t)
  (org-use-fast-todo-selection t)
  (org-default-notes-file "~/Documents/org/gsd/inbox.org")
  (org-directory "~/Documents/org/")
  (org-agenda-files '("~/Documents/org/gsd/gsd.org"))
  (org-refile-use-outline-path 'file org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets
   '(("gsd.org" :maxlevel . 1)
     ("someday.org" :maxlevel . 1)))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "APPT(a)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "CANCELLED(c@/!)")))
  (org-capture-templates
   '(("t" "Todo" entry (file org-default-notes-file) "* TODO %? \nAdded: %U\n")
     ("n" "Next" entry (file org-default-notes-file) "* NEXT %? \nDEADLINE: %t")
     ("j" "Journal" entry
      (file+olp+datetree "~/Documents/org/journal.org") "* %?\n" :clock-in t :clock-resume t)))
  (org-tag-alist
   (quote (("@errand" . ?e) ("@mari" . ?m) ("@reading" . ?r) ("@computer" . ?c)
           ("@work" . ?w)
           ("@home" . ?h))))
  (org-fast-tag-selection-single-key nil)

  (org-todo-keyword-faces
   '(("TODO" :foreground "light coral" :weight bold)
     ("NEXT" :foreground "red" :weight bold)
     ("DONE" :foreground "sea green")
     ("APPT" :foreground "maroon")
     ("WAITING" :foreground "dark orange" :weight bold)
     ("CANCELLED" :foreground "dim gray")
     ("HOLD" :foreground "deep sky blue" :weight bold)))
  (org-pretty-entities t))

(use-package org-pomodoro
  :after org
  :custom
  (org-pomodoro-format "%s"))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  (org-ellipsis "⤵")
  (org-bullets-bullet-list '("■" "◆" "▲" "▶")))

;; Git
(use-package magit
  :defer 3
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (git-commit-summary-max-length 50)
  :config
  (add-hook 'git-commit-mode-hook
            '(lambda () (setq fill-column 72) (turn-on-auto-fill))))

(use-package evil-magit :after evil magit)

(use-package git-timemachine
  :after evil magit
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Ledger
(use-package ledger-mode
  :custom
  (ledger-clear-whole-transactions 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode "\\.dat\\'")

                                        ; Code
;; Javascript
(use-package json-mode
  :general
  (:keymaps 'json-mode-map
   :states 'motion
   :prefix "SPC m"
   "f" 'json-mode-beautify))

(use-package add-node-modules-path
  :config
  ;; TODO(kevin) Refactor the hooks to async load
  (add-hook 'rjsx-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'add-node-modules-path))

(use-package prettier-js
  :after add-node-modules-path
  :config
  ;; TODO(kevin) Refactor the hooks to async load
  (add-hook 'rjsx-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode))

(use-package rjsx-mode
  :general
  (:keymaps 'rjsx-mode-map
   :states 'motion
   :prefix "SPC m"
   "r" 'tide-refactor
   "e" 'tide-rename-symbol
   "c" 'tide-rename-file)
  :custom
  (js2-basic-offset 2)
  (js2-mode-toggle-warnings-and-errors nil)
  (js2-mode-show-strict-warnings nil)
  :init
  ;; TODO(kevin) Try tsx with typescript-mode and not rjsx-mode
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package typescript-mode
  :general
  (:keymaps 'typescript-mode-map
   :states 'motion
   :prefix "SPC m"
   "r" 'tide-refactor
   "e" 'tide-rename-symbol
   "c" 'tide-rename-file)
  :custom
  (typescript-indent-level 2))

(use-package tide
  :defer 0.5
  :preface
  (defun setup-tide-mode ()
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))
  :config
  ;; TODO(kevin) Refactor the hooks to async load
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (css-indent-offset 2))

(use-package emmet-mode
  :delight
  :hook ((web-mode js2-mode js-mode) . emmet-mode))

(use-package rainbow-mode
  :delight
  :hook (prog-mode-hook . rainbow-mode))

;; Rust
(use-package rust-mode
  :general
  (:keymaps 'rust-mode-map
   :states 'motion
   :prefix "SPC m"
   "f" 'rust-format-buffer
   "b" 'cargo-process-build
   "r" 'cargo-process-run
   "t" 'cargo-process-test)
  :mode ("\\.rs\\'" . rust-mode))

(use-package flycheck-rust
  :after flycheck rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;; C/C++
(use-package ggtags
    :disabled
    :commands ggtags-mode
    :config
    (unbind-key "M-<" ggtags-mode-map)
    (unbind-key "M->" ggtags-mode-map))

(use-package cc-mode
    :disabled
    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (ggtags-mode 1)))))

;; (use-package lsp-mode)

;;  (use-package emacs-cquery
;;    :commands lsp-cquery-enable
;;    :init (setq cquery-executable "~/Programs/cquery/bin/cquery")
;;    (add-hook 'c-mode-hook #'cquery//enable)
;;    (add-hook 'c++-mode-hook #'cquery//enable))

;; Other crap
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

(use-package yaml-mode :mode "\\.yml\\'")

                                        ; Specific UI/UX
(use-package fill-column-indicator
  :hook (text-mode . fci-mode))

(use-package nlinum-relative
  :after evil
  :hook (prog-mode . nlinum-relative-mode)
  :config (nlinum-relative-setup-evil))

(use-package spacemacs-theme :defer t)
(use-package challenger-deep-theme :defer t)
(use-package zenburn-theme :defer t)
(load-theme 'challenger-deep t)

(use-package powerline
  :defer 2
  :config (powerline-center-evil-theme))

(use-package dimmer
  :init (dimmer-mode)
  :custom
  (dimmer-fraction 0.5))

(use-package focus
  :init (focus-mode))

;; The lines under this one are generated by Emacs, do not edit them!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 3)
 '(css-indent-offset 2 t)
 '(git-commit-summary-max-length 50 t)
 '(ivy-count-format "%d/%d")
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(js2-basic-offset 2 t)
 '(js2-mode-show-strict-warnings nil t)
 '(js2-mode-toggle-warnings-and-errors nil t)
 '(ledger-clear-whole-transactions 1 t)
 '(magit-completing-read-function (quote ivy-completing-read) t)
 '(markdown-command "multimarkdown" t)
 '(org-agenda-files (quote ("~/Documents/org/gsd/gsd.org")) t)
 '(org-bullets-bullet-list (quote ("■" "◆" "▲" "▶")))
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file org-default-notes-file)
      "* TODO %?
Added: %U
")
     ("n" "Next" entry
      (file org-default-notes-file)
      "* NEXT %?
DEADLINE: %t")
     ("j" "Journal" entry
      (file+olp+datetree "~/Documents/org/journal.org")
      "* %?
" :clock-in t :clock-resume t))) t)
 '(org-default-notes-file "~/Documents/org/gsd/inbox.org" t)
 '(org-directory "~/Documents/org/" t)
 '(org-ellipsis "⤵")
 '(org-fast-tag-selection-single-key nil t)
 '(org-hide-emphasis-markers t t)
 '(org-pomodoro-format "%s")
 '(org-pretty-entities t t)
 '(org-refile-allow-creating-parent-nodes (quote confirm) t)
 '(org-refile-targets
   (quote
    (("gsd.org" :maxlevel . 1)
     ("someday.org" :maxlevel . 1))) t)
 '(org-refile-use-outline-path (quote file) t)
 '(org-src-fontify-natively t t)
 '(org-tag-alist
   (quote
    (("@errand" . 101)
     ("@mari" . 109)
     ("@reading" . 114)
     ("@computer" . 99)
     ("@work" . 119)
     ("@home" . 104))) t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "light coral" :weight bold)
     ("NEXT" :foreground "red" :weight bold)
     ("DONE" :foreground "sea green")
     ("APPT" :foreground "maroon")
     ("WAITING" :foreground "dark orange" :weight bold)
     ("CANCELLED" :foreground "dim gray")
     ("HOLD" :foreground "deep sky blue" :weight bold))) t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "APPT(a)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "CANCELLED(c@/!)"))) t)
 '(org-use-fast-todo-selection t t)
 '(package-selected-packages
   (quote
    (dashboard lsp-mode powerline zenburn-theme challenger-deep-theme spacemacs-theme nlinum-relative fill-column-indicator yaml-mode ggtags cargo racer flycheck-rust rainbow-mode emmet-mode web-mode tide typescript-mode prettier-js add-node-modules-path ledger-mode git-timemachine evil-magit magit org-bullets org-pomodoro dictionary flycheck yasnippet-snippets yasnippet company counsel-projectile counsel-etags counsel ivy-rich ivy exec-path-from-shell general which-key evil-escape evil-surround evil-collection origami evil no-easy-keys delight use-package)))
 '(projectile-completion-system (quote ivy))
 '(projectile-switch-project-ation (quote projectile-dired) t)
 '(typescript-indent-level 2 t)
 '(web-mode-attr-indent-offset 2 t)
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-css-indent-offset 2 t)
 '(web-mode-markup-indent-offset 2 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
