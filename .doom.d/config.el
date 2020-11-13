;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Kevin De Baerdemaeker"
      user-mail-address "me@venikx.com")

(setq display-line-numbers-type t)
(setq lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; (setq doom-theme 'doom-challenger-deep)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-outrun-electric)
(setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-spacegray)

;; Everything org-mode
(setq org-directory "~/org/gsd")
(setq org-tag-alist
      (quote (("@errand" . ?e) ("@mari" . ?m) ("@reading" . ?r) ("@computer" . ?c)
              ("@work" . ?w)
              ("@home" . ?h))))

(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+modified:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%04y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp))

;; Standardizes the slug to use dashes instead of underscores
(after! (org org-roam)
  (defun org-roam--title-to-slug (title)
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("--*" . "-")  ;; remove sequential underscores
                      ("^-" . "")  ;; remove starting underscore
                      ("-$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))

  (setq org-roam-directory "~/org/braindump")
  (setq org-roam-capture-templates
        '(("n" "Regular" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n#+modified: %U\n\n"
           :unnarrowed t t)
          ("b" "Book" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "references/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: book\n#+created: %U\n#+modified: %U\n\n* Metadata\n- Author: \n- Source: \n-Reason: \n* Notes\n* Highlights"
           :unnarrowed t t)
          ("a" "Article" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "references/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: article\n#+created: %U\n#+modified: %U\n\n* Metadata\n- Author: \n- Source: \n-Reason: \n* Notes\n* Highlights"
           :unnarrowed t t)
          ("w" "Video" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "references/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: video\n#+created: %U\n#+modified: %U\n\n* Metadata\n- Author: \n- Source: \n-Reason: \n* Notes\n* Highlights"
           :unnarrowed t t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal/%<%Y>/%<%y%m%d>"
           :head "#+title: %<%A %B %-d, %Y>\n#+created: %U\n#+modified: %U\n\n"))))

;; (use-package! org
;;   :mode ("\\.org\\'" . org-mode)
;;   :init
;;   (setq org-return-follows-link t
;;         org-babel-load-languages '((emacs-lisp . t)))
;;
;;   (with-eval-after-load 'flycheck
;;     (flycheck-add-mode 'proselint 'org-mode)))
;;
;; org-id-store-link
;; org-roam-unlinked-references
;;
