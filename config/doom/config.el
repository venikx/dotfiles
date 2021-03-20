;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(global-auto-revert-mode t)

(setq
 user-full-name "Kevin Rangel"
 user-mail-address "code@venikx.com")

(setq
 doom-theme 'doom-laserwave
 ;;doom-theme 'doom-outrun-electric
 ;;doom-theme 'doom-dracula
 display-line-numbers-type t
 projectile-project-search-path '("~/code/")
 elfeed-feeds
 '("https://this-week-in-rust.org/rss.xml"))

;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(use-package! elcord :custom (elcord-use-major-mode-as-main-icon t))

(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

;; Everything org-mode
(setq
 deft-directory "~/org/braindump"
 deft-extensions '("org" "txt")
 deft-recursive t
 org-directory "~/org/gsd")

(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+modified:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp)
  (setq +org-capture-todo-file "capture.org"
        org-capture-templates
        '(("t" "todo" entry
           (file+headline +org-capture-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t :clock-in t :clock-resume t)

          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t :clock-in t :clock-resume t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend t :clock-in t :clock-resume t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a" :prepend t :clock-in t :clock-resume t)

          ("c" "Centralized templates for projects")
          ("ct" "Project todo" entry
           #'+org-capture-central-project-todo-file "* TODO %?\n%i\n%a" :heading "Tasks" :prepend t :clock-in t :clock-resume t)
          ("cn" "Project notes" entry
           #'+org-capture-central-project-notes-file "* %U %?\n%i\n%a" :heading "Notes" :prepend t :clock-in t :clock-resume t)
          ("cc" "Project changelog" entry
           #'+org-capture-central-project-changelog-file "* %U %?\n%i\n%a" :heading "Changelog" :prepend t :clock-in t :clock-resume t))))

;; Standardizes the slug to use dashes instead of underscores
(after! org-roam
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
        '(("n" "Note" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n#+modified: %U\n\n"
           :unnarrowed t t)
          ("l" "Literature Note" plain
           (function org-roam--capture-get-point)
           "* Metadata\n- Author: %^{Author}\n- Reason: %^{Reason}\n* Notes\n** %?\n* Highlights"
           :file-name "literature-notes/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: %^{Tags}\n#+roam_key: %^{Bibliographic Reference}\n#+created: %U\n#+modified: %U\n\n"
           :unnarrowed t t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal/%<%Y>/%<%y%m%d>"
           :head "#+title: %<%A %B %-d, %Y>\n#+created: %U\n#+modified: %U\n\n"))))

(defun venikx/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(use-package! org-agenda
  :init
  (setq org-agenda-use-time-grid nil
        org-agenda-custom-commands '((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'day)
                                                (org-deadline-warning-days 31)))
                                       (tags "PRIORITY=\"A\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                                              (org-agenda-overriding-header "Important")))
                                       (alltodo ""
                                                ((org-agenda-skip-function
                                                  '(or (venikx/org-skip-subtree-if-priority ?A)
                                                       (org-agenda-skip-if nil '(scheduled deadline))))
                                                 (org-agenda-overriding-header "Backlog")))
                                       )))))
