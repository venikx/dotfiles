;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Kevin De Baerdemaeker"
      user-mail-address "me@venikx.com")

(setq company-idle-delay nil)

(setq lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; (setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-spacegray)

(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(setq org-directory "~/org/")
(after! org
  (setq time-stamp-active t
        time-stamp-line-limit 4
        time-stamp-start "#\\+MODIFIED:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%04y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hooks 'time-stamp))

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
        '(("n" "note" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %U\n#+MODIFIED: %U\n\n"
           :unnarrowed t t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal/%<%Y>/%<%y%m%d>"
           :head "#+TITLE: %<%A %B %-d, %Y>\n#+CREATED: %U\n#+MODIFIED: %U\n\n"))))

(load "~/.doom.d/biblio.el")
(setq! +biblio-default-bibliography-files '("~/org/braindump/biblio.bib")
       +biblio-notes-path "~/org/braindump/references/")

(defun venikx/bibtex-completion-format-org-ref (keys)
  "Format org-ref references for keys in KEYS."
  (s-join ", "
          (--map (format "cite:%s" it) keys)))

;; Set the default cite format
(after! bibtex-completion
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . venikx/bibtex-completion-format-org-ref)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))))

(defun my/open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(after! org-ref
  (setq org-ref-show-broken-links nil)
  (setq org-ref-open-pdf-function 'my/open-pdf-at-point))

(after! org-roam-bibtex
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "references/${=KEY=}"
             :head "#+TITLE: ${author-or-editor}'s \"${title}\"\n#+ROAM_KEY: ${ref}\n#+CREATED: %U\n#+MODIFIED: %U\n\n%?"
             :unnarrowed t t)
          ("notes" "ref + noter" plain (function org-roam-capture--get-point)
             ""
             :file-name "references/${=KEY=}"
             :head "#+TITLE: ${author-or-editor}'s \"${title}\"\n#+ROAM_KEY: ${ref}\n#+CREATED: %U\n#+MODIFIED: %U\n\n%?\n\n* Annotations\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n"
             :unnarrowed t t))))

;; Open or create a note
(defun my/org-ref-ivy-open-notes (&optional arg)
  "Open the note of reference selected using ivy-bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg org-ref-default-bibliography
                               (org-ref-find-bibliography)))
  (ivy-read "Open/create note: " (orhc-bibtex-candidates)
            :require-match t
            :action 'my/ivy-bibtex-open-notes))

(defun my/ivy-bibtex-open-notes (entry)
  "Open the notes associated with ENTRY selected from `orhc-bibtex-candidates'."
    (org-ref-open-notes-at-point (cdr (assoc "=key=" entry))))

;; Open a PDF
(defun my/org-ref-ivy-open-pdf (&optional arg)
  "Open the PDF of reference selected using ivy-bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg org-ref-default-bibliography
                               (org-ref-find-bibliography)))
  (ivy-read "Open pdf: " (orhc-bibtex-candidates)
            :require-match t
            :action 'my/ivy-bibtex-open-pdf))

(defun my/ivy-bibtex-open-pdf (entry)
  "Open the PDF associated with ENTRY selected from `orhc-bibtex-candidates'."
  (with-ivy-window
    (let ((pdf (car (bibtex-completion-find-pdf (cdr (assoc "=key=" entry))))))
         (if (file-exists-p pdf)
           (org-open-file pdf)
           (message "No pdf found for %s" (cdr (assoc "=key=" entry)))))))

;; Open a bibtex entry
(defun my/org-ref-ivy-open-entry (&optional arg)
  "Open the bibtex entry of reference selected using ivy-bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg org-ref-default-bibliography
                               (org-ref-find-bibliography)))
  (ivy-read "Open entry: " (orhc-bibtex-candidates)
            :require-match t
            :action 'or-ivy-bibtex-open-entry))

;; Copy a citation key
(defun my/org-ref-ivy-copy-entry (&optional arg)
  "Copy the bibtex entry of reference selected using ivy-bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg org-ref-default-bibliography
                               (org-ref-find-bibliography)))
  (ivy-read "Open entry: " (orhc-bibtex-candidates)
            :require-match t
            :action 'or-ivy-bibtex-copy-entry))

;; Open a URL
(defun my/org-ref-ivy-open-url (&optional arg)
  "Open the URL of reference selected using ivy-bibtex."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg org-ref-default-bibliography
                               (org-ref-find-bibliography)))
  (ivy-read "Open entry: " (orhc-bibtex-candidates)
            :require-match t
            :action 'or-ivy-bibtex-open-url))

;; Add the custom functions to the bibliography key-chords.
(after! (org org-ref bibtex-completion)
  (map! :leader
    (:prefix ("n b" . "bibliography")
     :desc "Insert/update citation" "i" #'org-ref-ivy-insert-cite-link
     :desc "Activate ivy-bibtex" "I" #'ivy-bibtex))
  (map! :leader
      :desc "Open/create note" "n b n" #'my/org-ref-ivy-open-notes
      :desc "Open bibtex entry" "n b o" #'my/org-ref-ivy-open-entry
      :desc "Open PDF" "n b p" #'my/org-ref-ivy-open-pdf
      :desc "Open URL" "n b u" #'my/org-ref-ivy-open-url
      :desc "Copy bibtex entry" "n b y" #'my/org-ref-ivy-copy-entry))

(after! (org org-roam-bibtex)
  (map! :leader
        :desc "ORB note actions" "n b a" #'orb-note-actions))

(use-package nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

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
