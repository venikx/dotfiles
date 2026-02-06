;;; early-init.el --- Kevin De Baerdemaeker early-init -*- lexical-binding: t; -*-

;;; Commentary:
;;; Kevin De Baerdemaeker's early-init.el file

;;; Code:
(setq package-enable-at-startup nil) ;; I use nix pkgs for Emacs

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      inhibit-x-resources t)

;;; early-init.el ends here
