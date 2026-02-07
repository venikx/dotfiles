;; init.el --- Kevin De Baerdemaeker init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Felt like trying out Emacs 30, and see if I can get
;;; away with a bare minimum configuration. Written from scratch.
;;; Kevin De Baerdemaeker's Emacs configuration.
;;;
;;; Requirements:
;;;   - evil mode: neovim is still good tho
;;;   - org-mode: only reason I'm not using neovim
;;;   - minimal: keep maintance burden small

;;; Code 
;;; Bootstrap
(setq use-package-always-ensure nil) ; I only use pkgs defined in nix config
(use-package diminish)

;;; What I'd call "sane" defaults for Emacs
(use-package emacs
  :init
  (setq use-short-answers t
	custom-file (make-temp-file "emacs-custom.el") ; don't want local adjustments to be saved 
	ring-bell-function 'ignore
	help-window-select t
	inhibit-splash-screen t
	inhibit-startup-screen t
	inhibit-startup-buffer-menu t
        backup-by-copying t
        backup-directory-alist '((cons "." (file-name-concat user-emacs-directory "backup/")))
	create-lockfiles nil)
  :config
  (setq-default truncate-lines t
		indent-tabs-mode nil)          ; Use spaces instead of tabs
  (electric-pair-mode 1)
  (auto-save-visited-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (let ((font-name "Iosevka-12"))
    (set-frame-font font-name t t)
    (set-face-attribute 'default nil :family "Iosevka" :height 120))
  (setq-default mode-line-format
                '(" "
                  (:eval (propertize
                          (pcase (bound-and-true-p evil-state)
                            ('normal  " N ")
                            ('insert  " I ")
                            ('visual  " V ")
                            ('replace " R ")
                            ('emacs   " E ")
                            ('motion  " M ")
                            (_        " - "))
                          'face (pcase (bound-and-true-p evil-state)
                                  ('normal  `(:background ,(doom-color 'blue) :foreground ,(doom-color 'bg) :weight bold))
                                  ('insert  `(:background ,(doom-color 'green) :foreground ,(doom-color 'bg) :weight bold))
                                  ('visual  `(:background ,(doom-color 'magenta) :foreground ,(doom-color 'bg) :weight bold))
                                  ('replace `(:background ,(doom-color 'red) :foreground ,(doom-color 'bg) :weight bold))
                                  (_        `(:background ,(doom-color 'base5) :foreground ,(doom-color 'fg))))))
                  " "
                  (:eval (propertize (buffer-name) 'face 'font-lock-constant-face))
                  " %6l:%c (%o) "
                  (:eval (when vc-mode (concat " | ⇅ " (substring-no-properties vc-mode 5))))
                  mode-line-format-right-align
                  (:eval (concat "  " (symbol-name major-mode)))
                  "  " mode-line-misc-info))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p) ; filters M-x to show relevant to context
  (user-full-name "Kevin De Baerdemaeker"))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t)
  ;; dark themes
  (load-theme 'doom-solarized-dark-high-contrast t)
  ;;(load-theme 'doom-outrun-electric t)
  ;;(load-theme 'doom-tokyo-night t)
  ;; light themes
  ;;(load-theme 'doom-solarized-light t)
  ;;(load-theme 'doom-tomorrow-day t)
  ;;(load-theme 'doom-flatwhite t) ;; cool, but hard to isearch
  (set-face-attribute 'menu nil
                      :background (doom-color 'bg)
                      :foreground (doom-color 'fg)))


;;; Essentials
;; Helper functions
(defun my-future-function ()
  "TODO: Not implemented yet."
  (interactive)
  (message "Function not implemented yet."))

;; Keybindings using keymaps, and evil configuration
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))


(autoload 'consult-buffer "consult" t)
(defvar-keymap my-buffer-map
  "b" (cons "switch buffer" #'consult-buffer)
  "k" (cons "kill buffer" #'kill-current-buffer)
  "n" (cons "next buffer" #'next-buffer)
  "p" (cons "previous buffer" #'previous-buffer))

(defvar-keymap my-files-map
  "C" (cons "copy file" #'copy-file)
  "D" (cons "delete file" #'delete-file)
  "f" (cons "find file" #'find-file)
  "R" (cons "rename file" #'rename-file))

(defvar-keymap my-git-map
  "g" (cons "git status" #'magit-status))

(defvar-keymap my-project-map
  "b" (cons "switch buffer" #'consult-project-buffer)
  "f" (cons "find file" #'project-find-file)
  "s" (cons "switch project" #'project-switch-project))

(autoload 'consult-ripgrep "consult" t)
(defvar-keymap my-search-map
  "r" (cons "rg" #'consult-ripgrep)
  "t" (cons "dictionary" #'dictionary-search))

(defvar-keymap my-notes-map)

(defvar-keymap my-leader-map
  "b" (cons "buffer" my-buffer-map) 
  "c" (cons "code" #'my-future-function) 
  "f" (cons "files" my-files-map)
  "g" (cons "git" my-git-map) 
  "h" (cons "help" help-map)
  "n" (cons "notes" my-notes-map)
  "p" (cons "project" my-project-map)
  "s" (cons "search" my-search-map))

(use-package evil
  :custom (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-define-key '(normal visual motion) 'global (kbd "SPC") my-leader-map))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil vertico
  :bind
  (:map vertico-map
        ("C-j"  . #'vertico-next)
        ("C-M-j" . #'vertico-next-group)
        ("C-k"   . #'vertico-previous)
        ("C-M-k" . #'vertico-previous-group))
  :custom (evil-collection-setup-minibuffer nil)
  :config (evil-collection-init))

(use-package evil-goggles
  :after evil
  :hook (evil-mode . evil-goggles-mode)
  :config
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :after evil
  :hook (evil-mode . evil-commentary-mode))

;; Completion
(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :hook
  (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :demand t
  :after minibuffer
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
    :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :config
  (global-corfu-mode))

;; version control
(use-package magit
  :commands (magit-status magit-blame))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; Misc
(use-package autorevert
  :hook (after-init . global-auto-revert-mode) ; useful for when files change on disk
  :custom
  (auto-revert-verbose t))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)))

(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; Random tools and software
(use-package elcord
  :commands (elcord-mode))

(use-package nov
  :defer t
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(use-package gnuplot)

(use-package vterm
  :commands vterm)

(use-package osm
  :custom
  (osm-server 'default)
  (osm-copyright t))

;;; Programming Languages
;; Formatting
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package apheleia
  :hook ((javascript-mode . apheleia-mode)
	 (web-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)
	 (typescript-ts-mode . apheleia-mode)
	 (html-ts-mode . apheleia-mode)
	 (css-ts-mode . apheleia-mode)
	 (bash-ts-mode . apheleia-mode)
	 (nix-ts-mode . apheleia-mode))
  :custom
  (apheleia-formatters-respect-indent-level nil))

;; Code snippets
(use-package eldoc :diminish eldoc-mode)
(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :init
  (setq yas-snippet-dirs
        '("~/.config/emacs/snippets"))
  :config
  (yas-reload-all))

;; Language servers
(use-package eglot
  :hook ((json-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (c-or-c++-ts-mode . eglot-ensure)
         (csharp-ts-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (tsx-ts-mode . eglot-ensure)
	 (js-ts-mode . eglot-ensure)
	 (css-ts-mode . eglot-ensure)
	 (html-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(json-ts-mode . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
	       '(csharp-ts-mode . ("omnisharp" "-lsp")))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(html-ts-mode . ("vscode-html-language-server" "--stdio"))))

;; Treesitter
(use-package emacs
  :init
  (setq major-mode-remap-alist
        '((js-json-mode    . json-ts-mode)
          (c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (go-mode         . go-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (javascript-mode . js-ts-mode)
          (html-mode       . html-ts-mode)
          (css-mode        . css-ts-mode))))

;; Major Modes
(use-package javascript-mode
  :mode (("\\.mjs\\'" . javascript-mode)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)))

(use-package tsx-ts-mode
  :mode (("\\.jsx\\'" . tsx-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package web-mode
  :mode (("\\.eta\\'" . web-mode)
	 ("\\.astro\\'" . web-mode)))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)))

(use-package glsl-mode)

;;; Org-mode
(use-package org
  :config
  (require 'org-capture)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  :custom
  (org-edit-src-content-indentation 0)
  (org-agenda-files '("~/org/gtd" "~/org/collections"))
  (org-startup-folded 'content)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-refile 'time)
  (org-agenda-show-habits t)
  (org-babel-temporary-directory "~/.cache/emacs/babel-tmp/")
  (org-attach-method-type 'id)
  (org-attach-use-inheritance t)
  (org-attach-id-to-path-function-list
   '(org-attach-id-uuid-folder-format))
  :hook (before-save-hook . time-stamp)
  :config
  (unless (file-exists-p org-babel-temporary-directory)
    (make-directory org-babel-temporary-directory t))
  (defun my/org-mode-setup-time-stamp ()
    "Enable time-stamp only in Org mode."
    (setq-local time-stamp-active t
                time-stamp-start "#\\+modified:[ \t]*"
                time-stamp-end "$"
                time-stamp-format "[%Y-%m-%d %a %H:%M]")
    (add-hook 'before-save-hook #'time-stamp nil 'local))

  (add-hook 'org-mode-hook #'my/org-mode-setup-time-stamp)

  (keymap-set my-notes-map "c" (cons "capture" #'org-capture))

  (defvar-keymap my-org-leader-map
    "o" (cons "open link"     #'org-open-at-point)
    "i" (cons "create id"     #'org-id-get-create))
  (evil-define-key '(normal visual motion) org-mode-map
    (kbd "SPC m") my-org-leader-map)

  (setq org-capture-templates
	(append org-capture-templates
                '(("t" "todo" entry
		   (file+headline "gtd/capture.org" "Inbox")
                   "* TODO %?\n%i\n%F" :prepend t :clock-resume t)
                  ("j" "Journal" entry
                   (file+datetree "~/org/gtd/journal.org")
                   "* %U %?\n%i")
                  ("c" "Contact" entry
		   (file "gtd/contacts.org")
		   (file "~/org/templates/contact-entry.org")
                   :unnarrowed t))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org        . t)
     (http       . t)
     (graphql    . t)
     (mermaid    . t)
     (nix        . t)
     (css        . t)
     (typescript . t)
     (js         . t)
     (C          . t)
     (go         . t))))

(use-package ob-http       :after org) 
(use-package ob-graphql    :after org) 
(use-package ob-mermaid    :after org) 
(use-package ob-nix        :after org) 
(use-package ob-typescript :after org)
(use-package ob-go         :after org)

;; Notes
(use-package denote
  :demand t
  :after (org evil)
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory (expand-file-name "~/org/"))
  (setq denote-known-keywords '("programming" "emacs"))
  (setq denote-prompts '(title keywords template))
  (setq denote-templates
	;; TODO: replace this with interactie hidden org-capture templates
        '((note . "#+modified:   %U\n\n\n* References")))
  (setq denote-org-front-matter "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+signature:  %s
")

  (setq org-capture-templates
        (append org-capture-templates
                '(("d" "denote")
		  ("dn" "denote (plain)" plain
		   (file denote-last-path)
                   (function
                    (lambda ()
                      (let ((denote-use-template "#+modified:   %U\n\n%?\n\n* References")
			    (denote-org-capture-specifiers nil))
			(denote-org-capture-with-prompts :title :keywords))))
                   :unnarrowed t)
		  ("dw" "denote (work)" plain
		   (file denote-last-path)
                   (function
                    (lambda ()
                      (let ((denote-use-template "#+modified:   %U\n\n%?\n\n* References")
                            (denote-use-directory (expand-file-name "work" (denote-directory)))
			    (denote-org-capture-specifiers nil))
			(denote-org-capture-with-prompts :title :keywords))))
                   :unnarrowed t)
		  ("ds" "denote (source)" plain
		   (file denote-last-path)
                   (function
                    (lambda ()
                      (let ((denote-use-template "#+modified:   %U\n\n%?\n\n* Metadata
- Creator(s) :: %^{Creator(s)}
- [[%^{Link to Source}][Source]]
- Recommended By :: %^{Recommended By}
- Reason :: %^{Reason}")
                            (denote-use-directory (expand-file-name "references" (denote-directory)))
                            (denote-org-capture-specifiers nil))
			(denote-org-capture-with-prompts :title :keywords))))
                   :unnarrowed t))))

  (keymap-set my-notes-map "f" (cons "find note" #'denote-open-or-create))
  (keymap-set my-notes-map "i" (cons "insert note" #'denote-link-or-create))
  (keymap-set my-notes-map "n" (cons "open note" #'denote-link-open-at-point))
  (keymap-set my-notes-map "r" (cons "rename note" #'denote-rename-file))
  (keymap-set my-notes-map "d" (cons "query note" #'denote-dired))
  (keymap-set my-notes-map "b" (cons "backlinks" #'denote-backlink))
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package org-ql)

;; Nutrition
(use-package org
  :config
  (when (file-exists-p "~/org/collections/nutrition.org")
    (org-babel-load-file "~/org/collections/nutrition.org")))

;; Extras
(use-package org-contrib)

;(use-package org-contacts
;  :after org
;  :requires org-contrib
;  :init
;  (setq org-contacts-files '("~/org/gtd/contacts.org")))

(use-package org-checklist
  :after org
  :requires org-contrib)

(use-package org-download
  :after org
  :requires org-contrib
  :custom
  (org-download-screenshot-method "scrcap")
  (org-download-method 'attach)
  (org-download-timestamp "%Y%m%d-"))
