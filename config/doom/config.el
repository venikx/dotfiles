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
 standard-indent 2
 calendar-week-start-day 1 ;; Start on Monday
 projectile-project-search-path '("~/code/")
 elfeed-feeds '("https://blog.tecosaur.com/tmio/rss.xml"
                "https://weekly.nixos.org/feeds/all.rss.xml"
                ))

;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(use-package! elcord :custom (elcord-use-major-mode-as-main-icon t))

(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

;; Everything org-mode
(add-to-list 'org-modules 'org-checklist)

(setq
 deft-directory "~/org/braindump"
 deft-extensions '("org" "txt")
 deft-recursive t
 org-directory "~/org/gsd"
 org-contacts-files "~/org/braindump/braindump-private/contacts.org")

(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+modified:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (setq org-startup-folded 'content
        org-log-into-drawer t)
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

(use-package! org-roam
  :init
  (map! :leader
        :prefix "r"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  (setq org-roam-directory "~/org/braindump"
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  (setq org-roam-node-display-template
        "${title:*}  ${tags:20}")
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :config
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-unlinked-references-section))
  (org-roam-setup)
  (setq org-roam-capture-templates
        '(("n" "Note" plain
           "%?\n\n* Metadata\n- Tags :: \n- Related Notes :: "
           :if-new (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+title: ${title}\n#+created: %U\n#+modified: %U")
           :empty-lines 1
           :unnarrowed t)
          ("c" "Contact" entry "* ${title} :@:
:PROPERTIES:
:TYPE: person
:MOBILE: %^{+358 45 265 HELLO}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:ADDRESS: %^{Main Street, 00100 Helsinki, Finland}
:CREATED: %t
:ID: %(org-id-uuid)
:END:"
           :if-new (file "braindump-private/contacts.org")
           :unnarrowed t)
          ("l" "Literature" plain
           "* Metadata\n- Creator(s) :: \n- Origin :: \n- Recommended By :: \n- Reason :: \n* Notes\n** %?\n* Highlights"
           :if-new (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+roam_key: \n#+created: %U\n#+modified: %U")
           :empty-lines 1
           :immediate-finish t
           :unnarrowed t)))

  (setq org-roam-dailies-directory "braindump-private/journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "\n* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+created: %U\n#+modified: %U
\n* Morning Routine
** What happened yesterday?
** What is today's main purpose?
- [ ]
** What am I anxious about?
** What am I grateful for?
** What am I excited about?
* Highlights"))))

  (set-company-backend! 'org-mode '(company-capf)))

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
        org-agenda-start-on-weekday 1
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

;; Language Configuration
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
