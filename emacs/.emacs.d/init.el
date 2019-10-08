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

;; Initialize package repo's
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; workaround bug in Emacs 26
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package manager setup/install
(defvar package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(eval-and-compile
  (setq-default use-package-always-ensure t))

;; Dump custom-set-variables to a garbage file, do not load it! (Keeps init.el file clean for version control X_X)
(setq custom-file "~/.emacs.d/to-be-dumped.el")

;; Load the configuration from an org file and parse it via org-babel
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init)
;;; init.el ends here
