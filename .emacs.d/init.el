;;; init.el --- -*- lexical-binding: t -*-
;; Author: Kevin Rangel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; An opinionated Emacs configuration containing all the essentials I use, while programming
;;
;; The initialization file is only bootstrapping the "actual" config file, which is
;; documented in org files and passed through org-babel for Emacs to read from.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(let ((minver "27.0"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original) ;; defined in early-init.el
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (run-at-time
                                       1 nil (lamda ()  (setq gc-cons-threshold better-gc-cons-threshold)))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Initialize package repo's
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'cl-lib)
(setq package-quickstart t)
(package-initialize)

(defvar venikx/package-contents-refreshed nil)
(defun venikx/package-refresh-contents-once ()
  "Check for outdated packages and download them."
  (when (not venikx/package-contents-refreshed)
    (setq venikx/package-contents-refreshed t)
    (package-refresh-contents)))

(defun venikx/require-one-package (package)
  "Push the needed PACKAGE's onto a list, so they can be checked by venikx/package-refresh-contents-once."
  (when (not (package-installed-p package))
    (venikx/package-refresh-contents-once)
    (package-install package)))

(defun venikx/require (&rest packages)
  "Gather a list of PACKAGES to be required."
  (dolist (package packages)
    (venikx/require-one-package package)))

;; Load the configuration from an org file and parse it via org-babel
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Load the custom saved variables after loading the main configuration file through org-mode
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
