;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(global-auto-revert-mode t)

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
              ("@office" . ?o))))

(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+modified:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%04y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp))

(after! org
  (setq +org-capture-todo-file "breathe.org"
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
           #'+org-capture-central-project-changelog-file "* %U %?\n%i\n%a" :heading "Changelog" :prepend t :clock-in t :clock-resume t)))

  (setq org-icalendar-use-deadline '(event-if-not-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-todo-not-done event-if-not-todo todo-start)
        org-caldav-url "https://cloud.venikx.com/remote.php/dav/calendars/venikx"
        org-caldav-calendar-id "org-1"
        org-caldav-inbox "~/org/gsd/breathe.org"
        org-caldav-debug-level 2
        org-icalendar-timezone "Europe/Helsinki"
        org-caldav-files (list "~/org/gsd/personal.org"
                               "~/org/gsd/business.org"
                               "~/org/gsd/projects.org")))

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

;; Code
(setq +format-on-save-enabled-modes '(not js2-mode))
(add-hook! 'js2-mode-hook prettier-js-mode)
