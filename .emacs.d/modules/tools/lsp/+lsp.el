;;; tools/lsp/+lsp.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backends 'company-capf
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")


;;
;;; Packages

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat doom-etc-dir "lsp-session"))
  ;; For `lsp-clients'
  (setq lsp-server-install-dir (concat doom-etc-dir "lsp/"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; Let doom bind the lsp keymap.
  (when (featurep! :config default +bindings)
    (setq lsp-keymap-prefix nil))

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        ;; HACK Fix #2911, until it is resolved upstream. Links come in
        ;;      asynchronously from the server, but lsp makes no effort to
        ;;      "select" the original buffer before laying them down, so they
        ;;      could be rendered in the wrong buffer (like the minibuffer).
        lsp-enable-links t
        lsp-enable-text-document-color nil)

  ;; Disable features that modify our code without our permission.
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  :config
  (setq lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/")
        lsp-vetur-global-snippets-dir (expand-file-name "vetur"
                                                        (or (bound-and-true-p +snippets-dir)
                                                            (concat doom-private-dir "snippets/")))
        lsp-clients-emmy-lua-jar-path (concat lsp-server-install-dir "EmmyLua-LS-all.jar")
        lsp-xml-jar-file              (concat lsp-server-install-dir "org.eclipse.lsp4xml-0.3.0-uber.jar")
        lsp-groovy-server-file        (concat lsp-server-install-dir "groovy-language-server-all.jar"))

  (set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'lsp-mode :async t
    :documentation #'lsp-describe-thing-at-point
    :definition #'lsp-find-definition
    :implementations #'lsp-find-implementation
    :type-definition #'lsp-find-type-definition
    :references #'lsp-find-references)

  ;; REVIEW The '<leader> c l' prefix is hardcoded here, unfortunately.
  (when (featurep! :config default +bindings)
    (dolist (leader-key (list doom-leader-key doom-leader-alt-key))
      (let ((lsp-keymap-prefix (concat leader-key " c l")))
        (lsp-enable-which-key-integration))))

  (defadvice! +lsp--dont-auto-install-servers-a (orig-fn &rest args)
    "Don't auto-install LSP servers. Only complain if there aren't available."
    :around #'lsp
    ;; `lsp' is normally eager to automatically install LSP servers, or
    ;; prompting to do so, but (in my opinion) server installation should be a
    ;; deliberate act by the end-user:
    (letf! (defun lsp--client-download-server-fn (&rest _))
      (apply orig-fn args)))

  (add-hook! 'lsp-mode-hook
    (defun +lsp-init-optimizations-h ()
      "Increase `read-process-output-max' and `gcmh-high-cons-threshold'."
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (unless (boundp 'read-process-output-max)
        (setq-local read-process-output-max (* 1024 1024)))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its GC
      ;;        strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold))))
    (defun +lsp-display-guessed-project-root-h ()
      "Log what LSP things is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root.")))))

  (add-hook! 'lsp-completion-mode-hook
    (defun +lsp-init-company-backends-h ()
      (when lsp-completion-mode
        (set (make-local-variable 'company-backends)
             (cons +lsp-company-backends
                   (remove +lsp-company-backends
                           (remq 'company-capf company-backends)))))))

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (funcall orig-fn restart)
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (let ((lsp--cur-workspace workspace))
                     (unless (lsp--workspace-buffers lsp--cur-workspace)
                       (funcall orig-fn))))
             lsp--cur-workspace)))))


(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil)

  (when (featurep! +peek)
    (set-lookup-handlers! 'lsp-ui-mode :async t
      :definition 'lsp-ui-peek-find-definitions
      :implementations 'lsp-ui-peek-find-implementation
      :references 'lsp-ui-peek-find-references)))


(use-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)


(use-package! lsp-ivy
  :when (featurep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
